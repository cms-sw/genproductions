
CARDS=( \
BM1_JHEP04 \
BM2_JHEP04 \
BM3_JHEP04 \
BM4_JHEP04 \
BM5_JHEP04 \
BM6_JHEP04 \
BM7_JHEP04 \
BM8_JHEP04 \
BM9_JHEP04 \
BM10_JHEP04 \
BM11_JHEP04 \
BM12_JHEP04 \
BM8a_JHEP04 \
BM1_JHEP03 \
BM2_JHEP03 \
BM3_JHEP03 \
BM4_JHEP03 \
BM5_JHEP03 \
BM6_JHEP03 \
BM7_JHEP03 \
kl_0p00_kt_1p00_c2_1p00 \
kl_1p00_kt_1p00_c2_0p00 \
kl_1p00_kt_1p00_c2_0p35 \
kl_1p00_kt_1p00_c2_3p00 \
kl_5p00_kt_1p00_c2_0p00 \
)

for card in "${CARDS[@]}"; do
    cardname=powheg_ggHH_${card}.input
    cp template_card.input $cardname

    CHHH="NOTFOUND"
    CT="NOTFOUND"
    CTT="NOTFOUND"
    CGGH="NOTFOUND"
    CGGHH="NOTFOUND"

    if [ "$card" = "BM10_JHEP04" ]; then
        CHHH="10.0"
        CT="1.5"
        CTT="-1.0"
        CGGH="0.0"
        CGGHH="0.0"

   elif [ "$card" = "BM11_JHEP04" ]; then
        CHHH="2.4"
        CT="1.0"
        CTT="0.0"
        CGGH="0.666666"
        CGGHH="0.333333"

    elif [ "$card" = "BM12_JHEP04" ]; then
        CHHH="15.0"
        CT="1.0"
        CTT="1.0"
        CGGH="0.0"
        CGGHH="0.0"

    elif [ "$card" = "BM1_JHEP04" ]; then
        CHHH="7.5"
        CT="1.0"
        CTT="-1.0"
        CGGH="0.0"
        CGGHH="0.0"

    elif [ "$card" = "BM2_JHEP04" ]; then
        CHHH="1.0"
        CT="1.0"
        CTT="0.5"
        CGGH="-0.5333333333"
        CGGHH="-0.2"

    elif [ "$card" = "BM3_JHEP04" ]; then
        CHHH="1.0"
        CT="1.0"
        CTT="-1.5"
        CGGH="0.0"
        CGGHH="0.266666"

    elif [ "$card" = "BM4_JHEP04" ]; then
        CHHH="-3.5"
        CT="1.5"
        CTT="-3.0"
        CGGH="0.0"
        CGGHH="0.0"

    elif [ "$card" = "BM5_JHEP04" ]; then
        CHHH="1.0"
        CT="1.0"
        CTT="0.0"
        CGGH="0.53333333"
        CGGHH="0.33333333"

    elif [ "$card" = "BM6_JHEP04" ]; then
        CHHH="2.4"
        CT="1.0"
        CTT="0.0"
        CGGH="0.1333333333"
        CGGHH="0.0666666666"

    elif [ "$card" = "BM7_JHEP04" ]; then
        CHHH="5.0"
        CT="1.0"
        CTT="0.0"
        CGGH="0.1333333333"
        CGGHH="0.0666666666"

    elif [ "$card" = "BM8a_JHEP04" ]; then
        CHHH="1.0"
        CT="1.0"
        CTT="0.5"
        CGGH="0.26666666"
        CGGHH="0.0"

    elif [ "$card" = "BM8_JHEP04" ]; then
        CHHH="15.0"
        CT="1.0"
        CTT="0.0"
        CGGH="-0.666666"
        CGGHH="-0.333333"

    elif [ "$card" = "BM9_JHEP04" ]; then
        CHHH="1.0"
        CT="1.0"
        CTT="1.0"
        CGGH="-0.4"
        CGGHH="-0.2"

    elif [ "$card" = "BM1_JHEP03" ]; then
        CHHH="3.94"
        CT="0.94"
        CTT="-0.33333"
        CGGH="0.5"
        CGGHH="0.33333"

    elif [ "$card" = "BM2_JHEP03" ]; then
        CHHH="6.84"
        CT="0.61"
        CTT="0.33333"
        CGGH="0.0"
        CGGHH="-0.3333"

    elif [ "$card" = "BM4_JHEP03" ]; then
        CHHH="2.79"
        CT="0.61"
        CTT="0.33333"
        CGGH="-0.5"
        CGGHH="0.16667"

    elif [ "$card" = "BM6_JHEP03" ]; then
        CHHH="5.68"
        CT="0.83"
        CTT="0.33333"
        CGGH="-0.5"
        CGGHH="0.33333"

    elif [ "$card" = "BM3_JHEP03" ]; then
        CHHH="2.21"
        CT="1.05"
        CTT="-0.333333333"
        CGGH="0.5"
        CGGHH="0.5"

    elif [ "$card" = "BM5_JHEP03" ]; then
        CHHH="3.95"
        CT="1.17"
        CTT="-0.333333333"
        CGGH="0.166666"
        CGGHH="-0.5"

    elif [ "$card" = "BM7_JHEP03" ]; then
        CHHH="-0.1"
        CT="0.94"
        CTT="1.0"
        CGGH="0.166666"
        CGGHH="-0.166666"

    elif [ "$card" = "kl_0p00_kt_1p00_c2_1p00" ]; then
        CHHH="0.0"
        CT="1.0"
        CTT="1.0"
        CGGH="0.0"
        CGGHH="0.0"

    elif [ "$card" = "kl_1p00_kt_1p00_c2_0p00" ]; then
        CHHH="1.0"
        CT="1.0"
        CTT="0.0"
        CGGH="0.0"
        CGGHH="0.0"

    elif [ "$card" = "kl_1p00_kt_1p00_c2_0p35" ]; then
        CHHH="1.0"
        CT="1.0"
        CTT="0.35"
        CGGH="0.0"
        CGGHH="0.0"

    elif [ "$card" = "kl_1p00_kt_1p00_c2_3p00" ]; then
        CHHH="1.0"
        CT="1.0"
        CTT="3.0"
        CGGH="0.0"
        CGGHH="0.0"

    elif [ "$card" = "kl_5p00_kt_1p00_c2_0p00" ]; then
        CHHH="5.0"
        CT="1.0"
        CTT="0.0"
        CGGH="0.0"
        CGGHH="0.0"
    fi

    echo card $card
    echo $CHHH $CT $CTT $CGGH $CGGHH
    
    sed -i "s#<CHHH>#${CHHH}#g" $cardname
    sed -i "s#<CT>#${CT}#g" $cardname
    sed -i "s#<CTT>#${CTT}#g" $cardname
    sed -i "s#<CGGH>#${CGGH}#g" $cardname
    sed -i "s#<CGGHH>#${CGGHH}#g" $cardname

done

