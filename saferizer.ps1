ls -r | match "\.hs$" | % {
    sed -i -e 's:\.unsafeWrite:.write:g' $_.fullname
    sed -i -e 's:\.unsafeRead:.read:g' $_.fullname
    sed -i -e 's:`V.unsafeIndex`:V.!:g' $_.fullname
    sed -i -e 's:`VS\.unsafeIndex`:VS.!:g' $_.fullname
    sed -i -e 's:BU.unsafeIndex:B.index:g' $_.fullname
    sed -i -e 's:V.unsafeIndex:(V.!):g' $_.fullname
    sed -i -e 's:VS.unsafeIndex:(VS.!):g' $_.fullname
    sed -i -e 's:unsafeIndex:!:g' $_.fullname
    sed -i -e 's:unsafeFreeze:freeze:g' $_.fullname
}
