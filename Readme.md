# SPSSINC RASCH
## Estimate a Rasch model for item response data.
 This procedure estimates the parameters of the Rasch model for item response data, using the rasch function from the R ltm package.

---
Requirements
----
- IBM SPSS Statistics 18 or later and the corresponding IBM SPSS Statistics-Integration Plug-in for R.

Note: See also the [STATS EXRASCH](https://github.com/IBMPredictiveAnalytics/STATS_EXRASCH) extension.

---
Installation intructions
----
1. Open IBM SPSS Statistics
2. Navigate to Utilities -> Extension Bundles -> Download and Install Extension Bundles
3. Search for the name of the extension and click Ok. Your extension will be available.

---
Tutorial
----

### Installation Location

Analyze →

&nbsp;&nbsp;Scale →

&nbsp;&nbsp;&nbsp;&nbsp;Rasch Model... 

### UI
<img width="700" alt="image" src="https://user-images.githubusercontent.com/19230800/196486911-e7e02a5c-cb4d-458c-a3c1-fff39d0cbc83.png">
<img width="354" alt="image" src="https://user-images.githubusercontent.com/19230800/196486968-c0e33c29-65ac-4104-b322-9c176ce9a7ec.png">
<img width="619" alt="image" src="https://user-images.githubusercontent.com/19230800/196486992-a3294169-b03e-4465-a050-d36e51168967.png">

### Syntax
Example

> SPSSINC RASCH <br />
>   /VARIABLES ITEMS=educ jobcat <br />
>   /OPTIONS MISSING = LISTWISE EXECUTE=TRUE <br />
>   /PRINT  <br />
>   /PLOT  <br />
>   /SAVE.



---
License
----

- Apache 2.0
                              
Contributors
----

  - IBM SPSS
