import numpy as np

def make_string(arr,global_n):
	str_='''
launch
	set FS0 0
	set FS1 0
	set FS2 0
	set FM0 '''+arr[0]+'''
	set FM1 '''+arr[1]+'''
	set FM2 '''+arr[2]+'''
	set FM3 '''+arr[3]+'''
	set FM4 '''+arr[4]+'''
	set FM5 '''+arr[5]+'''
	set FM6 '''+arr[6]+'''
	set FM7 '''+arr[7]+'''
	set FT0 '''+arr[8]+'''
	set FT1 '''+arr[9]+'''
	set FT2 '''+arr[10]+'''
	set FT3	0
	set FT4	0
	set FT5 '''+arr[11]+'''
	set FT6 '''+arr[12]+'''
	set FT7 '''+arr[13]+'''
	set FT8 0
	set FT9 0
'''
	print(str_)


FM0s=['-4e-10','-3e-10','-2e-10','-1e-10','-5e-11','-2e-11','2e-11','5e-11','1e-10','2e-10','3e-10','4e-10']
FM1s=['-4e-10','-3e-10','-2e-10','-1e-10','-5e-11','-2e-11','2e-11','5e-11','1e-10','2e-10','3e-10','4e-10']
FM2s=['-4e-10','-3e-10','-2e-10','-1e-10','-5e-11','-2e-11','2e-11','5e-11','1e-10','2e-10','3e-10','4e-10']
FM3s=['-4e-10','-3e-10','-2e-10','-1e-10','-5e-11','-2e-11','2e-11','5e-11','1e-10','2e-10','3e-10','4e-10']
FM7s=['-8e-11','-6e-11','-4e-11','-2e-11','-1e-11','-5e-12','-3e-12','-1e-12','1e-12','3e-12','5e-12','1e-11','2e-11','4e-11','6e-11','8e-11']
FM4s=['-3e-11','-2e-11','-1e-11','-5e-12','-4e-12','-3e-12','-2e-12','-1e-12','1e-12','2e-12','3e-12','4e-12','5e-12','1e-11','2e-11','3e-11']
FM5s=['-3e-11','-2e-11','-1e-11','-5e-12','-4e-12','-3e-12','-2e-12','-1e-12','1e-12','2e-12','3e-12','4e-12','5e-12','1e-11','2e-11','3e-11']
FT0s=['-3e-11','-2e-11','-1e-11','-5e-12','-4e-12','-3e-12','-2e-12','-1e-12','1e-12','2e-12','3e-12','4e-12','5e-12','1e-11','2e-11','3e-11']
FT1s=['-3e-11','-2e-11','-1e-11','-5e-12','-4e-12','-3e-12','-2e-12','-1e-12','1e-12','2e-12','3e-12','4e-12','5e-12','1e-11','2e-11','3e-11']
FT2s=['-3e-11','-2e-11','-1e-11','-5e-12','-4e-12','-3e-12','-2e-12','-1e-12','1e-12','2e-12','3e-12','4e-12','5e-12','1e-11','2e-11','3e-11']
FT5s=['-3e-11','-2e-11','-1e-11','-5e-12','-4e-12','-3e-12','-2e-12','-1e-12','1e-12','2e-12','3e-12','4e-12','5e-12','1e-11','2e-11','3e-11']
FT6s=['-3e-11','-2e-11','-1e-11','-5e-12','-4e-12','-3e-12','-2e-12','-1e-12','1e-12','2e-12','3e-12','4e-12','5e-12','1e-11','2e-11','3e-11']
FT7s=['-3e-11','-2e-11','-1e-11','-5e-12','-4e-12','-3e-12','-2e-12','-1e-12','1e-12','2e-12','3e-12','4e-12','5e-12','1e-11','2e-11','3e-11']


first_string='''
change rwgt_dir rwgt


launch
    set FS0 0
    set FS1 0
    set FS2 0
    set FM0 0
    set FM1 0
    set FM2 0
    set FM3 0
    set FM4 0
    set FM5 0
    set FM6 0
    set FM7 0
    set FT0 0
    set FT1 0
    set FT2 0
    set FT3 0
    set FT4 0
    set FT5 0
    set FT6 0
    set FT7 0
    set FT8 0
    set FT9 0
'''



print(first_string)


global_n=0
for v in FM0s:
	varr=[v,'0','0','0','0','0','0','0','0','0','0','0','0','0']
	make_string(varr,global_n)
	global_n+=1

for v in FM1s:
	varr=['0',v,'0','0','0','0','0','0','0','0','0','0','0','0']
	make_string(varr,global_n)
	global_n+=1

for v in FM2s:
	varr=['0','0',v,'0','0','0','0','0','0','0','0','0','0','0']
	make_string(varr,global_n)
	global_n+=1

for v in FM3s:
	varr=['0','0','0',v,'0','0','0','0','0','0','0','0','0','0']
	make_string(varr,global_n)
	global_n+=1

for v in FM4s:
	varr=['0','0','0','0',v,'0','0','0','0','0','0','0','0','0']
	make_string(varr,global_n)
	global_n+=1

for v in FM5s:
	varr=['0','0','0','0','0',v,'0','0','0','0','0','0','0','0']
	make_string(varr,global_n)
	global_n+=1

for v in FM7s:
	varr=['0','0','0','0','0','0','0',v,'0','0','0','0','0','0']
	make_string(varr,global_n)
	global_n+=1

for v in FT0s:
	varr=['0','0','0','0','0','0','0','0',v,'0','0','0','0','0']
	make_string(varr,global_n)
	global_n+=1

for v in FT1s:
	varr=['0','0','0','0','0','0','0','0','0',v,'0','0','0','0']
	make_string(varr,global_n)
	global_n+=1

for v in FT2s:
	varr=['0','0','0','0','0','0','0','0','0','0',v,'0','0','0']
	make_string(varr,global_n)
	global_n+=1

for v in FT5s:
	varr=['0','0','0','0','0','0','0','0','0','0','0',v,'0','0']
	make_string(varr,global_n)
	global_n+=1

for v in FT6s:
	varr=['0','0','0','0','0','0','0','0','0','0','0','0',v,'0']
	make_string(varr,global_n)
	global_n+=1

for v in FT7s:
	varr=['0','0','0','0','0','0','0','0','0','0','0','0','0',v]
	make_string(varr,global_n)
	global_n+=1

