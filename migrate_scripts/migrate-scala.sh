FN=$1
if [ x$SED == x ]; then
    SED=sed
fi
$SED -i 's/newClientAgent/newContext/g' $FN
$SED -i 's/deleteHandle/deleteValue/g' $FN
$SED -i 's/\bca\b/ctx/g' $FN
$SED -i 's/ctx\.close()/ctx.closeContext()/g' $FN
$SED -i 's/Handle/MuValue/g' $FN
$SED -i 's/putInt("@i32",/handleFromInt32(/g' $FN
$SED -i 's/putInt("@i64",/handleFromInt64(/g' $FN
$SED -i 's/putInt("@i\(\d\+\)"\s*,\s*\([^)]*\))/handleFromInt(\2, \1)/g' $FN
$SED -i 's/putFloat("@float",/handleFromFloat(/g' $FN
$SED -i 's/putDouble("@double",/handleFromDouble(/g' $FN
$SED -i 's/putConstant/handleFromConst/g' $FN
$SED -i 's/putGlobal/handleFromGlobal/g' $FN
$SED -i 's/putFunction/handleFromFunc/g' $FN
$SED -i 's/toInt(\(\w\+\),\s*\(signExt\s*=\s*\)\?true)/handleToSInt(\1.asInstanceOf[MuIntValue])/g' $FN
$SED -i 's/toInt(\(\w\+\))/handleToUInt(\1.asInstanceOf[MuIntValue])/g' $FN
$SED -i 's/toFloat/handleToFloat/g' $FN
$SED -i 's/toDouble/handleToDouble/g' $FN
$SED -i 's/toPointer/handleToPtr/g' $FN
$SED -i 's/refCast/refcast/g' $FN
$SED -i 's/currentInstruction/curInst/g' $FN
$SED -i 's/TrapRebindPassVoid/returnFromTrap/g' $FN
$SED -i 's/TrapRebindPassValue(\(\w\+\),\s*\(\w\+\)\s*)/Rebind(\1, PassValues(Seq(\2)))/g' $FN

