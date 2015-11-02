FN=$1
if [ x$SED == x ]; then
    SED=sed
fi
$SED -i 's/\(%\w\+\)\s*=\s*TRAP/[\1] TRAP/g' $FN
$SED -i 's/NEWSTACK\s*<\(@[0-9a-zA-Z._-]\+\)>\s*\(@\w\+\)/COMMINST @uvm.new_stack <[\1]> (\2)/g' $FN
$SED -i 's/COMMINST\s*@uvm\.new_thread\s*(\([@%]\w\+\))/NEWTHREAD \1 PASS_VALUES /g' $FN
$SED -i 's/TRAP\s*<@void>/TRAP <>/g' $FN
$SED -i 's/noparamsnoret/v_v/g' $FN
$SED -i 's/@funcdumb/@frv_v/g' $FN
$SED -i '/\.funcsig/ {s/@void\s*(\([^)]*\))/(\1) -> ()}/g' $FN
$SED -i '/\.funcsig/ {s/=\s*\(@\w\+\)\s*(\([^)]*\))/= (\2) -> (\1)/}' $FN
$SED -i 's/@i_ii/@ii_i/g' $FN
$SED -i 's/RET @VOID/RET ()/g' $FN
$SED -i 's/hybrid\s*<@void\s*/hybrid</g' $FN

