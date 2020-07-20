untod --lzone 1 |sed  -e 's/$/ /'> .ref
ttod=`choose   0:2 -i .ref|sed -e 's/[ -]//g'`
tdate=`choose  4 -i .ref`
ttime=`choose  5 -i .ref`
tzone=`choose  6 -i .ref`
tjuli=`choose  7 -i .ref`
tday=`choose   8 -i .ref`
tpmc=`choose   9 -i .ref`
tunix=`choose 10 -i .ref`
tsec=$(( $tunix + 2208988800 ))
rule1=--------------------------------------------------------------------------------------------------------
rule2=========================================================================================================
{
    echo $rule2
    untod -vvv
    echo $rule1
    echo Local time reference for test:
    head -1 .ref|sed -e 's/^/  /'
    echo $rule2
    echo Timezone tests:
    for parm in \
        {"--rpad -o $ttod","-d $tdate@$ttime","-d $tjuli@$ttime","-m $tpmc","-u $tunix","-s $tsec"}
        do 
        rule=$rule2 
        for mode in {-g,-l,-t} 
            do 
            echo $rule
            echo untod -za $mode $parm
            untod -za --lzone=0 $mode $parm > .zulu
            untod -a --lzone=1 $mode $parm > .local
            python3 ndiff.py -q .zulu .local | sed -e 's/^ //'
            rule=$rule1
            done
        done
    echo $rule2
    echo Tickmode tests:
    for parm in \
        {"--rpad -o $ttod","-d $tdate@$ttime","-d $tjuli@$ttime","-m $tpmc","-u $tunix","-s $tsec"}
        do 
        rule=$rule2 
        echo $rule
        echo untod -a $mode $parm
        untod -a --lzone=0 -g $parm > .gmt
        untod -a --lzone=0 -l $parm > .loran
        untod -a --lzone=0 -t $parm > .tai
        python3 ndiff.py -q .gmt .loran | sed -e 's/^ //' 
        python3 ndiff.py -q .loran .tai | sed -e 's/^ //' |tail -2
        rule=$rule1
        done
    echo $rule2
} | sed -e 's/\x0D//g;/^ *$/d'