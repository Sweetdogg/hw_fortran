#! /bin/awk -f
BEGIN{
kai=0
c=0
q=0
l=4
}
{
kai=($3-$2^2)*l^2/$1
c=($5-$4^2)*l^2/$1^2
q=$3/$2
printf "%3i %5.3f %18.12f %18.12f %18.12f\n",l,$1,kai,c,q 
}
END{

}
