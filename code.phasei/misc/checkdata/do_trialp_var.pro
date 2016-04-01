pro do_trialp_var

  iterations = ['1','2','3','4']
  for iiter=0,n_elements(iterations) - 1 do begin
     iteration = iterations[iiter]
     
     field = 'x1i'
     
     dir = '~/Work/m33_mira/phot/i/'+field+'/'
     cd,dir
     f_sh = 'do_trialp_var_' + field + '.sh'
     f_do = 'do_trialp_var_' + field + '.do'
     f_out = 'do_trialp_var_' + field + '.out'
     f_log = 'do_trialp_var_' + field + '.log'
     spawn,'rm -f '+f_sh+' '+f_do+' '+f_out+' '+f_log
     openw,l_sh,f_sh,/get_lun
     printf,l_sh, 'set field = '+field
     printf,l_sh, 'set fieldi = $field'
     printf,l_sh, 'set vardir = trialp_var_$field'
     printf,l_sh, 'mkdir -p $vardir'
     printf,l_sh, 'rm -rf $vardir'
     printf,l_sh, 'mkdir -p $vardir'
     printf,l_sh, ''
     if (iteration eq '1') then begin
        printf,l_sh, 'mv $fieldi.frc orig/'
     endif
     printf,l_sh, 'rm -f $fieldi.fnl $fieldi.vry $fieldi.zer $fieldi.per'
     printf,l_sh, ''
     printf,l_sh, 'trialp < ' + f_do + ' > ' + f_out
     printf,l_sh, ''
     printf,l_sh, 'set i = 10'
     printf,l_sh, 'while ($i <= 99)'
     printf,l_sh, '  mv lcV.$i* $vardir'
     printf,l_sh, '  @ i++'
     printf,l_sh, 'end'
     printf,l_sh, 'mv lcV.* $vardir'
     printf,l_sh, 'mv $fieldi.fnl $fieldi.vry $fieldi.zer $fieldi.per $vardir'
     close,l_sh
     free_lun,l_sh

     if file_test(f_out) then begin
        cmd = 'grep "Suggested sigma multiplier" ' + f_out
        spawn,cmd,outstring
        multiplier = repstr(outstring,'Suggested sigma multiplier (1) ','')
     endif else begin
        multiplier = '1'
     endelse

     openw,l_do,f_do,/get_lun
     printf,l_do, field + '.tfr'
     printf,l_do, ''
     printf,l_do, ''
     printf,l_do, ''
     printf,l_do, ''
     printf,l_do, multiplier
     printf,l_do, '0.6, 5, 30, 30'
     printf,l_do, ''
     close,l_sh
     free_lun,l_sh

     openw,l_log, f_log, /get_lun, /append
     printf,l_log, 'multiplier =  ' + multiplier
     close,l_log
     free_lun,l_log

     print,'it_' + iteration  + ' Running TRIALP for field ' + field + ' ...'
     if (multiplier ne '') then begin
        spawn,'tcsh ' + f_sh
     endif
  endfor
end


