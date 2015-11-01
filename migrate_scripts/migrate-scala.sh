FN=$1
sed -i 's/newClientAgent/newContext/g' $FN
sed -i 's/\bca\b/ctx/g' $FN
sed -i 's/ctx\.close()/ctx.closeContext()/g' $FN
sed -i 's/putInt("@i32",/i32(/g' $FN
sed -i 's/putInt("@i64",/i64(/g' $FN
sed -i 's/putConstant/handleFromConst/g' $FN
sed -i 's/putGlobal/handleFromGlobal/g' $FN
sed -i 's/putFunction/handleFromFunc/g' $FN
sed -i 's/toInt(\(\w\+\),\s*signExt\s*=\s*true)/handleToSInt(\1.asInstanceOf[MuIntValue])/g' $FN
sed -i 's/currentInstruction/curInst/g' $FN
sed -i 's/TrapRebindPassVoid/returnFromTrap/g' $FN
sed -i 's/TrapRebindPassValue(\(\w\+\),\s*\(\w\+\)\s*)/Rebind(\1, PassValues(Seq(\2)))/g' $FN

