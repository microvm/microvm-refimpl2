FN=$1

sed -i 's/\(%\w\+\)\s*=\s*TRAP/[\1] TRAP/g' $FN
sed -i 's/NEWSTACK\s*<\(@\w\+\)>\s*\(@\w\+\)/COMMINST @uvm.new_stack <[\1]> (\2)/g' $FN
sed -i 's/COMMINST\s*@uvm\.new_thread\s*(\([@%]\w\+\))/NEWTHREAD \1 PASS_VALUES /g' $FN
sed -i 's/TRAP\s*<@void>/TRAP <>/g' $FN
sed -i 's/noparamsnoret/v_v/g' $FN
sed -i 's/@void\s*(\([^)]*\))/(\1) -> ()/g' $FN

