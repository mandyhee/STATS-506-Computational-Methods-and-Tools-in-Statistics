/*question 3*/
/*a*/
proc import datafile = 'C:\Users\mandyho\Desktop\Medicare_Provider_Util_Payment_PUF_CY2016.txt' 
	out = q3 
	dbms = tab replace; 
	getnames = yes; 
run;
proc print data = q3_b(obs=10); run;


proc contents data = tmp1.data; run;

data tmp1.data;
set tmp1.data;
hcpcs_code = input(hcpcs_code, 8.);
run;

/*b*/
data q3_b;
	set tmp1.data;
	where hcpcs_description contains "MRI"; 
	if hcpcs_code >= 70000 and hcpcs_code < 80000;
	srvc_payment = line_srvc_cnt*average_Medicare_payment_amt;
run;



/*c. use proc means*/
proc means data = q3_b noprint;
	class hcpcs_description;
	var line_srvc_cnt average_Medicare_payment_amt;
	output out = summary
            sum(line_srvc_cnt) = total_volume
            sum(srvc_payment) = total_payment;
run;

data summary1;
set summary;
average_payment = total_payment / total_volume;
run;


proc print data = summary1; run;


/*get the highest*/
data summary1; set summary1; if hcpcs_description = ' ' then delete; run;

proc means data = summary1 max;
	var average_payment total_payment total_volume;
run;

proc print data = summary1; run;

/*average_payment = 269.17664596
total_payment = 134223519.40
total_volume = 1430104.4 */


data ps4_q3;
set summary1;
where hcpcs_description in ('MRI scan of lower spinal canal', 'MRI scan of one breast with contrast');
run;



proc print data = ps4_q3; run;

/*d. use sql*/
proc sql;
	create table q3d_1 as
	select HCPCS_DESCRIPTION, sum(LINE_SRVC_CNT) as total_volume, sum(LINE_SRVC_CNT*AVERAGE_MEDICARE_PAYMENT_AMT) as total_payment
	from tmp1.data 
    where HCPCS_DESCRIPTION like "%MRI%" and HCPCS_CODE like "7%" 
	group by HCPCS_DESCRIPTION;
quit;


proc sql;
	create table q3d_2 as
	select HCPCS_DESCRIPTION, total_volume, total_payment, total_payment / total_volume as average_payment
	from q3d_1;
quit; 


/*get the highest*/
proc sql;
    create table ps4_q3d as
    select HCPCS_DESCRIPTION, total_volume, total_payment, average_payment
    from q3d_2
	having total_volume = max(total_volume) or total_payment = max(total_payment) or average_payment = max(average_payment);
quit;
proc print data = ps4_q3d; run;


/*e. export to csv*/
proc export outfile = "C:\Users\mandyho\Desktop\ps4_q3c.csv"
	data = ps4_q3c
	dbms = csv
	replace;
run;

proc export outfile = "C:\Users\mandyho\Desktop\ps4_q3d.csv"
	data = ps4_q3d
	dbms = csv
	replace;
run;


/*highest volume: sum(LINE_SRVC_CNT) by procedure
highest total payment: sum(LINE_SRVC_CNT)*AVERAGE_MEDICARE_PAYMENT_AMT (per procedure)
highest average payment: weighted sum of AVERAGE_MEDICARE_PAYMENT_AMT = highest total payment / sum(LINE_SRVC_CNT) overall*/






