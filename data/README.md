
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

* **BRPL**:

***actual energy = Sum(meters) - Less MES + Import from ROHTAK***

Note: Less MES = MES Sum(H:N); Import from ROHTAK = Rohtak Road Sum(AU:BF) + Apportionment in BRPL


* **BYPL**:
	
***actual energy = Sum(meters) + Import from ROHTAK***

Note: Import from ROHTAK = Rohtak Road Sum(BG:BH) + Apportionment in BYPL


* **NDPL**:
	
***actual energy = Sum(meters) + Import from ROHTAK***

Note: Import from ROHTAK = Rohtak Road Sum(BG:BH) + Apportionment in NDPL


* **NDMC**:

***actual energy = Sum(meters)***


* **MES**:

***actual energy = Sum(meters)***

The equations are tested to be correct by compaing a sample week's actual demand and calculated demand.


### 1.2 Approximate hourly discom demand using meter reads summation.

Since "Import from ROHTAK" and "Less MES" are relatively small numbers compared with the sum of meter reads, we use the summation to approximate hourly discom demand.


### 1.3 Compare approximation using mapping provided in "company wise details" with actual demand

 