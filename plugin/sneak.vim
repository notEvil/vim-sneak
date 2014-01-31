" sneak.vim - The missing motion
" Author:       Justin M. Keyes
" Version:      1.6.1
" License:      MIT

if exists('g:loaded_sneak_plugin') || &compatible || v:version < 700
  finish
endif
let g:loaded_sneak_plugin = 1

let s:cpo_save = &cpo
set cpo&vim

"persist state for repeat
let s:st = { 'rst':1, 'input':'', 'op':'', 'reverse':0, 'count':0, 'bounds':[0,0] }

func! sneak#init()
  "options                                 v-- for backwards-compatibility
  let s:opt = { 'f_reset' : get(g:, 'sneak#nextprev_f', get(g:, 'sneak#f_reset', 1))
      \ ,'t_reset'      : get(g:, 'sneak#nextprev_t', get(g:, 'sneak#t_reset', 1))
      \ ,'textobject_z' : get(g:, 'sneak#textobject_z', 1)
      \ ,'use_ic_scs'   : get(g:, 'sneak#use_ic_scs', 0)
      \ ,'map_netrw'    : get(g:, 'sneak#map_netrw', 1)
      \ ,'streak'       : get(g:, 'sneak#streak', 0) && (v:version >= 703) && has("conceal")
      \ }

  for k in ['f', 't'] "if user mapped f/t to Sneak, then disable f/t reset.
    if maparg(k, 'n') =~# 'Sneak'
      let s:opt[k.'_reset'] = 0
    endif
  endfor
endf

call sneak#init()

func! sneak#opt()
  return deepcopy(s:opt)
endf

func! sneak#state()
  return deepcopy(s:st)
endf

func! s:is_sneaking()
  return exists("#SneakPlugin#CursorMoved#<buffer>")
endf

" convenience wrapper for key bindings/mappings
func! sneak#wrap(op, input_length, reverse, streak) range abort
  " don't repeat the last 's' search if this is an 'f' search, etc.
  "TODO: check inclusive/exclusive when we add support for that
  let is_similar_invocation = a:input_length == ((v:version >= 703) ? strwidth(s:st.input) : len(s:st.input))
  "TRICKY: use v:prevcount for visual mapping because we <esc> before the ex command.
  let l:count = max([1, sneak#util#isvisualop(a:op) ? v:prevcount : v:count1])

  if s:is_sneaking() && is_similar_invocation " 's' goes to next match
    call sneak#rpt(a:op, l:count, a:reverse)
  else " 's' invokes new search
    call sneak#to(a:op, s:getnchars(a:input_length, a:op), l:count, 0, a:reverse, [0,0], a:streak)
  endif
endf

"repeat *motion* (not operation)
func! sneak#rpt(op, count, reverse) range abort
  if s:st.rst "reset by f/F/t/T
    exec "norm! ".(sneak#util#isvisualop(a:op) ? "gv" : "").a:count.(a:reverse ? "," : ";")
    return
  endif

  call sneak#to(a:op, s:st.input, a:count, 1,
        \ ((a:reverse && !s:st.reverse) || (!a:reverse && s:st.reverse)), s:st.bounds, 0)
endf

func! sneak#to(op, input, count, repeatmotion, reverse, bounds, streak) range abort "{{{
  if empty(a:input) "user canceled
    redraw | echo '' | return
  endif

  "highlight tasks:
  "  - highlight actual matches at or below (above) the cursor position
  "  - highlight the vertical 'tunnel' that the search is scoped-to

  let s = g:sneak#search#instance
  call s.init(s:opt, a:input, a:repeatmotion, a:reverse)
  let streak_mode = 0

  " [count] means 'skip this many' (_only_ on repeat-motion).
  "   sanity check: max out at 999, to avoid searchpos() OOM.
  let skip = a:repeatmotion ? min([999, a:count]) : 0

  let l:gt_lt = a:reverse ? '<' : '>'
  let l:bounds = deepcopy(a:bounds) " [left_bound, right_bound]
  " pattern used to highlight the vertical 'scope'
  let l:scope_pattern = ''
  let l:match_bounds  = ''

  "scope to a column of width 2*(v:count1) _unless_ this is a repeat-motion.
  if ((!skip && a:count > 1) || max(l:bounds) > 0) && (empty(a:op) || sneak#util#isvisualop(a:op))
    " use provided bounds if any, otherwise derive bounds from range
    if max(l:bounds) <= 0
      "these are the _logical_ bounds highlighted in 'scope' mode
      let l:bounds[0] =  max([0, (virtcol('.') - a:count - 1)])
      let l:bounds[1] =  a:count + virtcol('.') + 1
    endif
    "matches *all* chars in the scope.
    "important: use \%<42v (virtual column) instead of \%<42c (byte column)
    let l:scope_pattern .= '\%>'.l:bounds[0].'v\%<'.l:bounds[1].'v'
  endif

  if max(l:bounds) > 0
    "adjust logical left-bound for the _match_ pattern by -len(s) so that if _any_
    "char is within the logical bounds, it is considered a match.
    let l:leftbound = max([0, (bounds[0] - len(a:input)) + 1])
    let l:match_bounds   = '\%>'.l:leftbound.'v\%<'.l:bounds[1].'v'
    let s.match_pattern .= l:match_bounds
  endif

  if !a:repeatmotion "this is a new (not repeat) invocation
    "persist even if the search fails, because the _reverse_ direction might have a match.
    let s:st.rst = 0 | let s:st.input = a:input | let s:st.op = a:op | let s:st.count = a:count | let s:st.bounds = l:bounds | let s:st.reverse = a:reverse

    "set temporary hooks on f/F/t/T so that we know when to reset Sneak.
    call s:ft_hook()
  endif

  if !empty(a:op) && !sneak#util#isvisualop(a:op) "operator-pending invocation
    let l:histreg = @/
    let wrap = &wrapscan | let &wrapscan = 0

    try
      " invoke / and restore the history immediately after
      silent! exec 'norm! '.a:op.(a:reverse ? '?' : '/').(s.prefix).(s.search)."\<cr>"
      if a:op !=# 'y'
        let s:last_op = deepcopy(s:st)
        " repeat c as d (this matches Vim default behavior)
        if a:op =~# '^[cd]$' | let s:last_op.op = 'd' | endif
        silent! call repeat#set("\<Plug>SneakRepeat")
      endif
    catch E486
      call sneak#util#echo('not found: '.a:input) | return
    finally
      call histdel("/", histnr("/")) "delete the last search from the history
      let @/ = l:histreg
      let &wrapscan = wrap
    endtry
  else "jump to the first match, or exit
    for i in range(1, max([1, skip]))
      let matchpos = s.dosearch()
      if 0 == max(matchpos)
        break
      endif
    endfor

    if 2 == a:streak || (a:streak && s:opt.streak)
      "enter streak-mode iff there are >=2 _additional_ matches.
      let streak_mode = s.hasmatches(2)
    endif

    "if the user was in visual mode, extend the selection.
    if sneak#util#isvisualop(a:op)
      norm! gv
      if max(matchpos) > 0 | call cursor(matchpos) | endif
    endif

    if 0 == max(matchpos)
      call sneak#util#echo('not found'.((max(l:bounds) > 0) ? printf(' (in columns %d-%d): %s', l:bounds[0], l:bounds[1], a:input) : ': '.a:input))
      return
    endif
  endif
  "search succeeded

  call sneak#hl#removehl()

  "position _after_ completed search
  let l:curlin = string(line('.'))
  let l:curcol = string(virtcol('.') + (a:reverse ? -1 : 1))

  "Might as well scope to window height (+/- 99).
  let l:top = max([0, line('w0')-99])
  let l:bot = line('w$')+99
  let l:restrict_top_bot = '\%'.l:gt_lt.l:curlin.'l\%>'.l:top.'l\%<'.l:bot.'l'
  let l:scope_pattern .= l:restrict_top_bot
  let s.match_pattern .= l:restrict_top_bot
  let l:curln_pattern  = l:match_bounds.'\%'.l:curlin.'l\%'.l:gt_lt.l:curcol.'v'

  if max(l:bounds) > 0 "perform the scoped highlight...
    let w:sneak_sc_hl = matchadd('SneakPluginScope', l:scope_pattern, 1, get(w:, 'sneak_sc_hl', -1))
  endif

  call s:attach_autocmds()

  "perform the match highlight...
  "  - store in w: because matchadd() highlight is per-window.
  "  - re-use w:sneak_hl_id if it exists (-1 lets matchadd() choose).
  let w:sneak_hl_id = matchadd('SneakPluginTarget',
        \ (s.prefix).s.match_pattern.'\zs'.(s.search).'\|'.l:curln_pattern.(s.search),
        \ 2, get(w:, 'sneak_hl_id', -1))

  if streak_mode && 0 == max(l:bounds) "vertical-scope-mode takes precedence over streak-mode.
    call sneak#streak#to(s, s:st)
  endif

  return
endf "}}}

func! s:attach_autocmds()
  augroup SneakPlugin
    autocmd!
    autocmd InsertEnter,WinLeave,BufLeave <buffer> call sneak#hl#removehl() | autocmd! SneakPlugin * <buffer>
    "*nested* CursorMoved autocmd to skip the _first_ CursorMoved event.
    "NOTE: CursorMoved is _not_ triggered if there is 'typeahead', which means during a macro or other script...
    autocmd CursorMoved <buffer> autocmd SneakPlugin CursorMoved <buffer> call sneak#hl#removehl() | autocmd! SneakPlugin * <buffer>
  augroup END
endf

func! sneak#reset(key)
  let c = sneak#util#getchar()

  let s:st.rst = 1
  let s:st.reverse = 0
  for k in ['f', 't'] "unmap the temp mappings
    if s:opt[k.'_reset']
      silent! exec 'unmap '.k
      silent! exec 'unmap '.toupper(k)
    endif
  endfor

  "count is prepended implicitly by the <expr> mapping
  return a:key.c
endf

func! s:map_reset_key(key, mode)
  exec printf("%snoremap <silent> <expr> %s sneak#reset('%s')", a:mode, a:key, a:key)
endf

func! s:ft_hook() "set up temporary mappings to 'hook' into f/F/t/T
  for k in ['f', 't']
    for m in ['n', 'x']
      "if user mapped anything to f or t, do not map over it; unfortunately this
      "also means we cannot reset ; or , when f or t is invoked.
      if s:opt[k.'_reset'] && maparg(k, m) ==# ''
        call s:map_reset_key(k, m) | call s:map_reset_key(toupper(k), m)
      endif
    endfor
  endfor
endf

func! s:repeat_last_op()
  let st = s:last_op
  call sneak#to(st.op, st.input, st.count, 0, st.reverse, st.bounds, 0)
endf

func! s:getnchars(n, mode)
  let s = ''
  echo '>'
  for i in range(1, a:n)
    "preserve existing selection
    if sneak#util#isvisualop(a:mode) | exe 'norm! gv' | endif
    let c = sneak#util#getchar()
    if -1 != index(["\<esc>", "\<c-c>", "\<backspace>", "\<del>"], c)
      return ""
    endif
    if c == "\<CR>"
      if i > 1 "special case: accept the current input (#15)
        break
      else "special case: repeat the last search (useful for streak-mode).
        return s:st.input
      endif
    else
      let s .= c
      if &iminsert && (v:version >= 703) && strwidth(s) >= a:n
        "HACK: this can happen if the user entered multiple characters while we
        "were waiting to resolve a multi-char keymap.
        "example for keymap 'bulgarian-phonetic':
        "    e:: => ё    | resolved, strwidth=1
        "    eo  => eo   | unresolved, strwidth=2
        break
      endif
    endif
    redraw | echo '>'.s
  endfor
  return s
endf

func! s:cnt(...) "if an arg is passed, it means 'visual mode'
  return max([1, a:0 ? v:prevcount : v:count1])
endf

" DEPRECATED: these four commands will be removed in v2.0
command! -bar -bang -nargs=1 Sneak          call sneak#to('', <sid>getnchars(<args>, ''), <sid>cnt(), 0, 0, [0,0], <bang>1)
command! -bar -bang -nargs=1 SneakBackward  call sneak#to('', <sid>getnchars(<args>, ''), <sid>cnt(), 0, 1, [0,0], <bang>1)
command! -bar -bang -nargs=1 SneakV         call sneak#to(visualmode(), <sid>getnchars(<args>, visualmode()), <sid>cnt(1), 0, 0, [0,0], <bang>1)
command! -bar -bang -nargs=1 SneakVBackward call sneak#to(visualmode(), <sid>getnchars(<args>, visualmode()), <sid>cnt(1), 0, 1, [0,0], <bang>1)

" 2-char sneak
nnoremap <silent> <Plug>SneakForward   :<c-u>call sneak#wrap('', 2, 0, 1)<cr>
nnoremap <silent> <Plug>SneakBackward  :<c-u>call sneak#wrap('', 2, 1, 1)<cr>
nnoremap <silent> <Plug>SneakNext      :<c-u>call sneak#rpt('', <sid>cnt(), 0)<cr>
nnoremap <silent> <Plug>SneakPrevious  :<c-u>call sneak#rpt('', <sid>cnt(), 1)<cr>
xnoremap <silent> <Plug>VSneakForward  <esc>:<c-u>call sneak#wrap(visualmode(), 2, 0, 1)<cr>
xnoremap <silent> <Plug>VSneakBackward <esc>:<c-u>call sneak#wrap(visualmode(), 2, 1, 1)<cr>
xnoremap <silent> <Plug>VSneakNext     <esc>:<c-u>call sneak#rpt(visualmode(), <sid>cnt(1), 0)<cr>
xnoremap <silent> <Plug>VSneakPrevious <esc>:<c-u>call sneak#rpt(visualmode(), <sid>cnt(1), 1)<cr>

" 1-char sneak, inclusive
nnoremap <silent> <Plug>Sneakf      :<c-u>call sneak#wrap('', 1, 0, 0)<cr>
nnoremap <silent> <Plug>SneakF      :<c-u>call sneak#wrap('', 1, 1, 0)<cr>
xnoremap <silent> <Plug>Sneakf <esc>:<c-u>call sneak#wrap(visualmode(), 1, 0, 0)<cr>
xnoremap <silent> <Plug>SneakF <esc>:<c-u>call sneak#wrap(visualmode(), 1, 1, 0)<cr>
onoremap <silent> <Plug>Sneakf      :<c-u>call sneak#wrap(v:operator, 1, 0, 0)<cr>
onoremap <silent> <Plug>SneakF      :<c-u>call sneak#wrap(v:operator, 1, 1, 0)<cr>

" 1-char sneak, exclusive
nnoremap <silent> <Plug>Sneakt      :<c-u>call sneak#wrap('', 1, 0, 0)<cr>
nnoremap <silent> <Plug>SneakT      :<c-u>call sneak#wrap('', 1, 1, 0)<cr>
xnoremap <silent> <Plug>Sneakt <esc>:<c-u>call sneak#wrap(visualmode(), 1, 0, 0)<cr>
xnoremap <silent> <Plug>SneakT <esc>:<c-u>call sneak#wrap(visualmode(), 1, 1, 0)<cr>
onoremap <silent> <Plug>Sneakt      :<c-u>call sneak#wrap(v:operator, 1, 0, 0)<cr>
onoremap <silent> <Plug>SneakT      :<c-u>call sneak#wrap(v:operator, 1, 1, 0)<cr>

if s:opt.textobject_z
  nnoremap yz :<c-u>call sneak#to('y',        <sid>getnchars(2, 'y'), <sid>cnt(), 0, 0, [0,0], 0)<cr>
  nnoremap yZ :<c-u>call sneak#to('y',        <sid>getnchars(2, 'y'), <sid>cnt(), 0, 1, [0,0], 0)<cr>
  onoremap z  :<c-u>call sneak#to(v:operator, <sid>getnchars(2, v:operator), <sid>cnt(), 0, 0, [0,0], 0)<cr>
  onoremap Z  :<c-u>call sneak#to(v:operator, <sid>getnchars(2, v:operator), <sid>cnt(), 0, 1, [0,0], 0)<cr>
endif

nnoremap <silent> <Plug>SneakRepeat :<c-u>call <sid>repeat_last_op()<cr>
nnoremap <Plug>SneakStreak :<c-u>call sneak#to('', <sid>getnchars(2, ''), <sid>cnt(), 0, 0, [0,0], 2)<cr>
nnoremap <Plug>SneakStreakBackward :<c-u>call sneak#to('', <sid>getnchars(2, ''), <sid>cnt(), 0, 1, [0,0], 2)<cr>

if !hasmapto('<Plug>SneakForward') && mapcheck('s', 'n') ==# ''
  nmap s <Plug>SneakForward
endif
if !hasmapto('<Plug>SneakBackward') && mapcheck('S', 'n') ==# ''
  nmap S <Plug>SneakBackward
endif

if !hasmapto('<Plug>SneakNext') && mapcheck(';', 'n') ==# ''
  nmap ; <Plug>SneakNext
endif
if !hasmapto('<Plug>SneakPrevious')
  if mapcheck(',', 'n') ==# ''
    nmap , <Plug>SneakPrevious
  elseif mapcheck('\', 'n') ==# '' || mapcheck('\', 'n') ==# ','
    nmap \ <Plug>SneakPrevious
  endif
endif

if !hasmapto('<Plug>VSneakForward') && mapcheck('s', 'x') ==# ''
  xmap s <Plug>VSneakForward
endif
if !hasmapto('<Plug>VSneakBackward') && mapcheck('Z', 'x') ==# ''
  xmap Z <Plug>VSneakBackward
endif

if !hasmapto('<Plug>VSneakNext') && mapcheck(';', 'x') ==# ''
  xmap ; <Plug>VSneakNext
endif
if !hasmapto('<Plug>VSneakPrevious')
  if mapcheck(',', 'x') ==# ''
    xmap , <Plug>VSneakPrevious
  elseif mapcheck('\', 'x') ==# ''
    xmap \ <Plug>VSneakPrevious
  endif
endif

if s:opt.map_netrw && -1 != stridx(maparg("s", "n"), "Sneak")
  func! s:map_netrw_key(key)
    if -1 != stridx(maparg(a:key,"n"), "_Net")
      exec 'nnoremap <buffer> <silent> <leader>'.a:key.' '.maparg(a:key,'n')
      "unmap netrw's buffer-local mapping to allow Sneak's global mapping.
      silent! exe 'nunmap <buffer> '.a:key
    endif
  endf

  augroup SneakPluginNetrw
    autocmd!
    autocmd FileType netrw autocmd SneakPluginNetrw CursorMoved <buffer>
          \ call <sid>map_netrw_key('s') | call <sid>map_netrw_key('S') | autocmd! SneakPluginNetrw * <buffer>
  augroup END
endif


let &cpo = s:cpo_save
unlet s:cpo_save
