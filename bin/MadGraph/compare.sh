#!/bin/bash
awk -F":" 'NR==FNR { a[FNR]=$0; next }
{
	split(a[FNR],b,FS);
			if ($3 + 0 == 0) next;
					#printf("compare %s %s %f / %f = %f\n", b[1],b[2],b[3], $3, b[3]/$3)
					if ( b[3]/$3 < 0.90 )
						{
							printf(" \n WARNING!! The %s  below tolerance %f  \n", b[2],  b[3]/$3 )
						}
				  	if ( b[3]/$3 > 1.1 ){
						printf(" \n WARNING!! The %s  above tolerance %f \n", b[2] ,  b[3]/$3)
						}
				

			
				}' $1 $2
