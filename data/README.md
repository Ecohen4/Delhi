
---
Spatial Analysis of Electricity Demand Distribution in Delhi -- Data Details  
---

## Overview

	* Demand Data: 
		15-min meter reads
		hourly discom reads
	* Cencus Data: 
		population data
		househould data


## 1. Demand Data Details

### 1.1 Relation between 15-min meter reads and hourly discom demand.

* **BRPL**:  ***actual energy = Sum(meters) - Less MES + Import from ROHTAK***

Note: Less MES = MES Sum(H:N); Import from ROHTAK = Rohtak Road Sum(AU:BF) + Apportionment in BRPL


* **BYPL**:  ***actual energy = Sum(meters) + Import from ROHTAK***

Note: Import from ROHTAK = Rohtak Road Sum(BG:BH) + Apportionment in BYPL


* **NDPL**:  ***actual energy = Sum(meters) + Import from ROHTAK***

Note: Import from ROHTAK = Rohtak Road Sum(AL:AT) + Apportionment in NDPL


* **NDMC**:  ***actual energy = Sum(meters)***


* **MES**:  ***actual energy = Sum(meters)***

The equations are tested to be correct by compaing a sample week's actual demand and calculated demand.


### 1.2 Approximate hourly discom demand using meter reads summation.

Since "Import from ROHTAK" and "Less MES" are relatively small numbers compared with the sum of meter reads, we use the summation to approximate hourly discom demand.

[need to show comparison later]

### 1.3 "company wise details" approximation for actual demand

### 1.3.1 Net drawl facts in "company wise details"

* **BRPL**:

	*estimated energy = IP STATION BRPL + PAPPANKALAN-1 + PAPPANKALAN-2 + NAJAFGARH + LODHI ROAD + OKHLA + VASANT KUNJ + MEHRAULI + SARITA VIHAR*


* **BYPL**:

	*estimated energy = IP STATION + RPH + GT (BYPL) + KASHMIRI GATE + NARAINA (BYPL) + PARK STREET + SOW + PATPARGANJ + GEETA COLONY + GAZIPUR*
	

* **NDPL**:
	
	*estimated energy = GOPALPUR + UBZI MANDI + ROHINI + SHALIMARBAGH + NARELA + NARAINA (NDPL) + KASHMIRI GATE (DMRC)+ KANJHAWALA + BAWANA*


* **MES**:

	*estimated energy = NARAINA MES*


* **NDMC**:

	*lack information*


### 1.3.2 Approximation for actual demand advised by "company wise details"

Use discom bounded vonoroi for grid-discom mappnig:

* **BRPL**:

*estimated energy = PAPPANKALAN-1 + PAPPANKALAN-2 + NAJAFGARH + LODHI ROAD + OKHLA + VASANT KUNJ + MEHRAULI + SARITA VIHAR*


* **BYPL**:

*estimated energy = GEETA COLONY + KASHMIRI GATE + SOW + PATPARGANJ + GAZIPUR + NARAINA (BYPL)*


* **NDPL**:

*estimated energy = GOPALPUR + SUBZI MANDI + SHALIMARBAGH + NARELA + NARAINA (NDPL) + KANJHAWALA + BAWANA + ROHINI*


* **MES**:

*estimated energy = NARAINA MES*


* **NDMC**:

*estimated energy = PARK STREET*
