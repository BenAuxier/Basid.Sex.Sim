#This will count the number of parasites in the sample
#using column 8, then it will count the numbers. Ignore the second nuclei since it shold be the same

echo "file,non-zero,gt0.1,gt0.2,gt0.3,gt0.4,gt0.5,,gt0.6,gt0.7,gt0.8,gt0.90" > parasite_summary.txt
for i in m*.dat;
do printf "%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s\n" \
       $i \
       $(tail -n +20 $i | tr -s " " | cut -d " " -f 8 | perl -ne '$c++ if $_ > 0.0; END{print $c+0 ."\n"}') \
       $(tail -n +20 $i | tr -s " " | cut -d " " -f 8 | perl -ne '$c++ if $_ > 0.1; END{print $c+0 ."\n"}') \
       $(tail -n +20 $i | tr -s " " | cut -d " " -f 8 | perl -ne '$c++ if $_ > 0.2; END{print $c+0 ."\n"}') \
       $(tail -n +20 $i | tr -s " " | cut -d " " -f 8 | perl -ne '$c++ if $_ > 0.3; END{print $c+0 ."\n"}') \
       $(tail -n +20 $i | tr -s " " | cut -d " " -f 8 | perl -ne '$c++ if $_ > 0.4; END{print $c+0 ."\n"}') \
       $(tail -n +20 $i | tr -s " " | cut -d " " -f 8 | perl -ne '$c++ if $_ > 0.5; END{print $c+0 ."\n"}') \
       $(tail -n +20 $i | tr -s " " | cut -d " " -f 8 | perl -ne '$c++ if $_ > 0.6; END{print $c+0 ."\n"}') \
       $(tail -n +20 $i | tr -s " " | cut -d " " -f 8 | perl -ne '$c++ if $_ > 0.7; END{print $c+0 ."\n"}') \
       $(tail -n +20 $i | tr -s " " | cut -d " " -f 8 | perl -ne '$c++ if $_ > 0.8; END{print $c+0 ."\n"}') \
       $(tail -n +20 $i | tr -s " " | cut -d " " -f 8 | perl -ne '$c++ if $_ > 0.9; END{print $c+0 ."\n"}') >> parasite_summary.txt
done
