#! /bin/awk -f
BEGIN{
avm=0
asm=0
ave=0
ase=0
davm=0

bins=3 # change bins 

}

{
avm+=$2
davm+=$2^2

asm+=$3
#dasm+=$3^2

ave+=$4
#dave+=$4^2

ase+=$5
#dase+=$5^2
}

END{
delta=(davm-asm)/(bins*(bins-1))
printf "%5.3f %18.12f %18.12f %18.12f %18.12f %18.12f\n",$1,avm/bins,asm/bins,ave/bins,ase/bins,delta
#printf "%5.3f %18.12f\n",$1,((davm-2*avm*avm/bins+(avm/bins)^2*bins)/(bins*(bins-1)))*0.5     #claulate erro bar
} 

