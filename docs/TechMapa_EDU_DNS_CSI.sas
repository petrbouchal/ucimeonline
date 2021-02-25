/* propojeni tabulek z CSI a DNS a identifikace technologii MS a Google*/
proc sql;
create table edu.dns_all as
select redizo, red_izo, csi.instituce, csi.kraj, dns.kraj, dns.mesto, dns.psc, dns.castObce, dns.nazevskoly,  
CSI.Q233164,
CSI.Q233116,
CSI.Q233117_238648,
CSI.Q233117_238648,
CSI.Q233145, 
CSI.Q233148_238734 as Bakalari, 
CSI.Q233148_238735 AS Discord,
CSI.Q233148_238736 AS Edmodo,
CSI.Q233148_238737 AS Edookit,
CSI.Q233148_238738 AS Edupage,
CSI.Q233148_238739 AS Facebook,
CSI.Q233148_238740 AS Google_Classroom,
CSI.Q233148_238741 AS Hangout_meet,
CSI.Q233148_238742 AS Microsoft_Teams,
CSI.Q233148_238743 AS Moodle,
CSI.Q233148_238744 AS OBS_Studio,
CSI.Q233148_238745 AS Samepage,
CSI.Q233148_238746 AS Skype,
CSI.Q233148_238747 AS SMART_Learning_Suite,
CSI.Q233148_238748 AS Skola_OnLine,
CSI.Q233148_238749 AS Viber,
CSI.Q233148_238750 AS Webex_Cisco,
CSI.Q233148_238751 AS WhatsApp,
CSI.Q233148_238752 AS ZOOM,
dns.odhadovanasluzba AS DNS_SLUZBA
from edu.csi 
right join edu.dns 
on csi.redizo = dns.red_izo;
quit;

* vyhodnoceni shody dat z CSI a DNS; 
* pravidla pro interpretaci hodnot technologii;
data edu.sluzba_all (keep=redizo red_izo instituce nazevskoly kraj mesto castobce psc Microsoft_Teams Google_Classroom csi_sluzba dns_sluzba Shoda_MS Shoda_google shoda_celkem sluzba edookit); 
	set edu.dns_all;  
	format 	csi_sluzba $char20.
			dns_sluzba $char20.
			Shoda_MS $char20.
			Shoda_google $char20.
			shoda_celkem $char40.
			Sluzba $char40.
		;
	
	*technologie zahrnute do vyhodnoceni ;
	IF (Microsoft_Teams EQ 1) AND (Google_Classroom EQ 1) THEN CSI_sluzba="Google Clas. + MS Teams"; else
	IF Microsoft_Teams EQ 1 THEN CSI_sluzba="Microsoft Teams"; else
	IF Google_Classroom EQ 1 THEN CSI_sluzba="Google Classroom"; else
	IF ZOOM EQ 1 THEN CSI_sluzba="ZOOM"; else
	IF Webex_Cisco EQ 1 THEN CSI_sluzba="Webex - Cisco"; else
	IF WhatsApp EQ 1 THEN CSI_sluzba="WhatsApp"; else
	IF Facebook EQ 1 THEN CSI_sluzba="Facebook"; else
	IF Hangout_meet EQ 1 THEN CSI_sluzba="Google Hangout meet"; else
	IF Viber EQ 1 THEN CSI_sluzba="Viber"; else
	IF Skype EQ 1 THEN CSI_sluzba="Skype"; else
	IF Edmodo EQ 1 THEN CSI_sluzba="Edmodo"; 
	
	* vyhodnoceni hlavnich technologii. Jak ZOOM?;
	IF Microsoft_Teams EQ 1 AND	findw(DNS_Sluzba,'O365')>0 then do; Shoda_MS="Shoda DNS a CSI"; Sluzba="Microsoft platforma"; end;
	IF Microsoft_Teams EQ 1 AND	DNS_Sluzba Not EQ "MS Outlook (O365)" then do; Shoda_MS="CSI lepsi info"; Sluzba="Microsoft platforma"; end;
	IF (Microsoft_Teams EQ 0 or Microsoft_Teams EQ ".") AND	DNS_Sluzba EQ "MS Outlook (O365)" then do; Shoda_MS="DNS lepsi info"; Sluzba="Microsoft platforma"; end;
		
	IF Google_Classroom EQ 1 AND DNS_Sluzba EQ "G Suite" then do; Shoda_Google="Shoda DNS a CSI"; Sluzba="Google platforma"; end;
	IF Google_Classroom EQ 1 AND DNS_Sluzba Not EQ "G Suite" then do; Shoda_Google="CSI lepsi info"; Sluzba="Google platforma"; end;
	IF (Google_Classroom EQ 0 OR Google_Classroom EQ "." ) AND DNS_Sluzba EQ "G Suite" then do; Shoda_Google="DNS lepsi info"; Sluzba="Google platforma"; end;

	if Length(Shoda_MS)> 1 then do; Shoda_celkem=Shoda_MS; Sluzba="Microsoft platforma"; end;
	if Length(Shoda_google)> 1 then do; Shoda_celkem=Shoda_google; Sluzba="Google platforma"; end;
	if (dns_sluzba eq "Bez MX záznamu" OR dns_sluzba eq "Neznámý") AND Length (CSI_sluzba) EQ 1 then do; Shoda_celkem="Neznama sluzba"; Sluzba="Neznámá služba"; end;
	IF Microsoft_Teams EQ 1 AND	findw(DNS_Sluzba,'Google')>0 then do; Shoda_Celkem="DNS uvadi Google, CSI Microsoft"; Sluzba="DNS uvadi Google, CSI Microsoft"; end;
	IF Google_Classroom EQ 1 AND findw(DNS_Sluzba,'O365')>0 then do; Shoda_Celkem="DNS uvadi Microsoft, CSI Google";  Sluzba="DNS uvadi Microsoft, CSI Google"; end;
	if length(shoda_celkem) eq 1 then do; shoda_celkem="Minoritní technologie"; sluzba="Minoritní technologie"; end;
run;
