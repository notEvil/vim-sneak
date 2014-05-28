" NOTES:
"   problem:  cchar cannot be more than 1 character.
"   strategy: make fg/bg the same color, then conceal the other char.
" 
"   problem:  keyword highlighting always takes priority over conceal.
"   strategy: syntax clear | [do the conceal] | let &syntax=s:syntax_orig
"
" PROFILING:
"   - the search should be 'warm' before profiling
"   - searchpos() appears to be about 30% faster than 'norm! n'
"
" FEATURES:
"   - skips folds
"   - if no visible matches, does not invoke streak-mode
"   - there is no 'grouping'
"     - this minimizes the steps for the common case
"     - If your search has >56 matches, press <tab> to jump to the 57th match
"       and label the next 56 matches.
" 
" cf. EASYMOTION:
"   - EasyMotion's 'single line' feature is superfluous because streak-mode
"     isn't activated unless there are >=2 on-screen matches, and any key that
"     isn't a target falls through to Vim.
"   - because sneak targets 2 chars, there is never a problem discerning
"     target labels. https://github.com/Lokaltog/vim-easymotion/pull/47#issuecomment-10919205
"   - https://github.com/Lokaltog/vim-easymotion/issues/59#issuecomment-23226131
"     - easymotion edits the buffer, plans to create a new buffer
"     - 'the current way of highligthing is insanely slow'
"   - sneak handles long lines https://github.com/Lokaltog/vim-easymotion/issues/82
"   - sneak can find and highlight concealed characters

let g:sneak#target_labels = get(g:, 'sneak#target_labels', "asdfghjkl;qwertyuiopzxcvbnm/ASDFGHJKL:QWERTYUIOPZXCVBNM?")

func! s:placematch(c, pos)
  let s:matchmap[a:c] = a:pos
  exec "syntax match SneakStreakTarget '\\%".a:pos[0]."l\\%".a:pos[1]."c.' conceal cchar=".a:c
endf

func! s:decorate_statusline() "highlight statusline to indicate streak-mode.
  highlight! link StatusLine SneakStreakStatusLine
endf

func! s:restore_statusline() "restore normal statusline highlight.
  highlight! link StatusLine NONE
endf

func! sneak#streak#to(s, v, reverse)
  let seq = ""
  while 1
    let choice = s:do_streak(a:s, a:v, a:reverse)
    let seq .= choice
    if choice != "\<Tab>" | return seq | endif
  endwhile
endf

func! s:do_streak(s, v, reverse) "{{{
  let w = winsaveview()
  call s:before()
  let search_pattern = (a:s.prefix).(a:s.search).(a:s.get_onscreen_searchpattern(w))

  let i = 0
  let overflow = [0, 0] "position of the next match (if any) after we have run out of target labels.

  let tPos = getpos('.')
  call cursor(line('w0'), 1)
  while 1
    " searchpos() is faster than 'norm! /'
    let p = searchpos(search_pattern, a:s.search_options_no_s, a:s.get_stopline())
    let skippedfold = sneak#util#skipfold(p[0], a:reverse) "Note: 'set foldopen-=search' does not affect search().

    if 0 == p[0] || -1 == skippedfold
      break
    elseif 1 == skippedfold
      continue
    endif

    if i < s:maxmarks
      "TODO: multibyte-aware substring: matchstr('asdfäöü', '.\{4\}\zs.') https://github.com/Lokaltog/vim-easymotion/issues/16#issuecomment-34595066
      let c = strpart(g:sneak#target_labels, i, 1)
      call s:placematch(c, p)
    else "we have exhausted the target labels; grab the first non-labeled match.
      let overflow = p
      break
    endif

    let i += 1
  endwhile
  call cursor(tPos)

  call winrestview(w) | redraw
  let choice = sneak#util#getchar()
  call s:after()

  let mappedto = maparg(choice, a:v ? 'x' : 'n')
  let mappedtoNext = mappedto =~# '<Plug>SneakNext'

  if choice == "\<Tab>" && overflow[0] > 0 "overflow => decorate next N matches
    call cursor(overflow[0], overflow[1])
  elseif -1 != index(["\<Esc>", "\<C-c>"], choice)
    return "\<Esc>" "exit streak-mode.
  elseif !mappedtoNext && !has_key(s:matchmap, choice) "press _any_ invalid key to escape.
    call feedkeys(choice) "exit streak-mode and fall through to Vim.
    return ""
  else "valid target was selected
    let p = mappedtoNext ? s:matchmap[strpart(g:sneak#target_labels, 0, 1)] : s:matchmap[choice]
    call cursor(p[0], p[1])
  endif

  return choice
endf "}}}

func! s:after()
  autocmd! sneak_streak_cleanup * <buffer>
  silent! call matchdelete(w:sneak_cursor_hl)
  "remove temporary highlight links
  exec 'hi! link Conceal '.s:orig_hl_conceal
  exec 'hi! link SneakPluginTarget '.s:orig_hl_sneaktarget
  call s:restore_statusline()
  let &synmaxcol=s:synmaxcol_orig
  let &syntax=s:syntax_orig
  let &concealcursor=s:cc_orig
  let &conceallevel=s:cl_orig
  call s:restore_conceal_in_other_windows()
endf

func! s:disable_conceal_in_other_windows()
  for w in range(1, winnr('$'))
    if 'help' !=# getwinvar(w, '&buftype') && w != winnr()
      call setwinvar(w, 'sneak_orig_cl', getwinvar(w, '&conceallevel'))
      call setwinvar(w, '&conceallevel', 0)
    endif
  endfor
endf
func! s:restore_conceal_in_other_windows()
  for w in range(1, winnr('$'))
    if 'help' !=# getwinvar(w, '&buftype') && w != winnr()
      call setwinvar(w, '&conceallevel', getwinvar(w, 'sneak_orig_cl'))
    endif
  endfor
endf

func! s:before()
  let s:matchmap = {}

  " prevent highlighting in other windows showing the same buffer
  ownsyntax sneak_streak

  " highlight the cursor location (else the cursor is not visible during getchar())
  let w:sneak_cursor_hl = matchadd("SneakStreakCursor", '\%#', 11, -1)

  let s:cc_orig=&l:concealcursor | setlocal concealcursor=ncv
  let s:cl_orig=&l:conceallevel  | setlocal conceallevel=2

  let s:syntax_orig=&syntax
  syntax clear
  " this is fast since we cleared syntax, and it allows sneak to work on very long wrapped lines.
  let s:synmaxcol_orig=&synmaxcol | set synmaxcol=0

  let s:orig_hl_conceal = sneak#hl#links_to('Conceal')
  let s:orig_hl_sneaktarget = sneak#hl#links_to('SneakPluginTarget')
  "set temporary link to our custom 'conceal' highlight
  hi! link Conceal SneakStreakTarget
  "set temporary link to hide the sneak search targets
  hi! link SneakPluginTarget SneakStreakMask

  call s:disable_conceal_in_other_windows()
  call s:decorate_statusline()

  augroup sneak_streak_cleanup
    autocmd!
    autocmd CursorMoved <buffer> call <sid>after()
  augroup END
endf

"returns 1 if a:key is invisible or special.
func! s:is_special_key(key)
  return -1 != index(["\<Esc>", "\<C-c>", "\<Space>", "\<CR>", "\<Tab>"], a:key)
    \ || maparg(a:key, 'n') =~# '<Plug>Sneak\(Next\|Previous\)'
    \ || (g:sneak#opt.s_next && maparg(a:key, 'n') =~# '<Plug>Sneak\(_s\|Forward\)')
endf

"we must do this because:
"  - we don't know which keys the user assigned to SneakNext/Previous
"  - we need to reserve special keys like <Esc> and <Tab>
func! sneak#streak#sanitize_target_labels()
  let nrkeys = sneak#util#strlen(g:sneak#target_labels)
  let i = 0
  while i < nrkeys
    let k = strpart(g:sneak#target_labels, i, 1)
    if s:is_special_key(k) "remove the char
      let g:sneak#target_labels = substitute(g:sneak#target_labels, '\%'.(i+1).'c.', '', '')
      "move ; (or s if 'clever-s' is enabled) to the front.
      if (!g:sneak#opt.s_next && maparg(k, 'n') =~# '<Plug>SneakNext') || (maparg(k, 'n') =~# '<Plug>Sneak\(_s\|Forward\)')
        let g:sneak#target_labels = k . g:sneak#target_labels
      else
        let nrkeys -= 1
        continue
      endif
    endif
    let i += 1
  endwhile
endf

func! sneak#streak#init()
  call sneak#streak#sanitize_target_labels()
  let s:maxmarks = sneak#util#strlen(g:sneak#target_labels)
endf

call sneak#streak#init()


func! sneak#streak#mystreak(input)
  if empty(a:input) " user canceled
    redraw | echo '' | return 0
  endif

  " preparation
  if !hlexists('SneakStreakCursor') " workaround
    hi link SneakStreakCursor Cursor
  endif
  call s:before()

  "" define a bunch of variables
  let firstline = line('w0')
  let lastline = line('w$')

  " create pattern
  let prefix = sneak#search#get_cs(a:input, g:sneak#opt.use_ic_scs).'\V'
  let restrict_top_bot = '\%>'.(firstline - 1).'l\%<'.(lastline + 1).'l'
  let pattern = prefix.escape(a:input, '"\')

  " labels
  let labelsmain = 'fjdksla;' " home row
  let labelsabove = 'rueiwoqpty' " above home row
  let labelsbelow = 'ghvncmx,b' " between or below home row
  let labelsextra = '234567890' " 1 and l are not distinguishable

  let matches = []

  let curLine = line('.')
  let curCol = col('.')

  let w = winsaveview()
  call cursor(firstline, 1)

  "" find matches
  while 1
    let [l, c] = searchpos(pattern, 'W', lastline) " search, moves cursor
    let skippedfold = sneak#util#skipfold(l, 0) " move cursor to end of fold if inside one

    if l == 0 || skippedfold == -1 " no match
      break
    elseif skippedfold == 1
      continue
    endif

    " first value: euclidean dist (y stretched by 2), favors matches right of cursor a little bit
    call add(matches, [pow((l - curLine) * 2, 2) + pow(c - curCol, 2), l < curLine, [l, c]])
  endwhile

  call winrestview(w)

  if !len(matches)
    call sneak#util#echo('not found')
    return 0
  endif
  
  " highlight matches
  call sneak#hl#removehl()
  let w:sneak_hl_id = matchadd('SneakPluginTarget', restrict_top_bot.pattern)

  "" assign labels
  call sort(matches, function('s:compareFirst'))

  let imain = 0
  let iabove = 0
  let ibelow = 0
  let iextra = 0

  for [t, above, pos] in matches
    if imain < len(labelsmain)
      let c = strpart(labelsmain, imain, 1)
      let imain += 1

    elseif above && iabove < len(labelsabove)
      let c = strpart(labelsabove, iabove, 1)
      let iabove += 1

    elseif !above && ibelow < len(labelsbelow)
      let c = strpart(labelsbelow, ibelow, 1)
      let ibelow += 1

    elseif iextra < len(labelsextra)
      let c = strpart(labelsextra, iextra, 1)
      let iextra += 1

    else
      continue
    endif

    call s:placematch(c, pos)

  endfor

  redraw
  let choice = sneak#util#getchar()
  call s:after()

  if has_key(s:matchmap, choice)
    let p = s:matchmap[choice]
    call cursor(p[0], p[1])
  else
    call sneak#hl#removehl()
    return 0
  endif

  return 1
endf

func! s:compareFirst(a, b)
  let x = a:a[0]
  let y = a:b[0]
  if x < y
    return -1
  elseif x == y
    return 0
  else
    return 1
  endif
endf
