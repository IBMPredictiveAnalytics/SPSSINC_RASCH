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
>  /VARIABLES ITEMS=item1 item2 item3 item4 item5 <br />
>   /PRINT SUMMARY ITEMFIT <br />
>   /PLOT DESCRIPTIVES FACTORSCORES ICC IIC <br />
>   /SAVE PERSONFITDATASET=personfit.

### Output
<img width="358" alt="image" src="https://user-images.githubusercontent.com/19230800/196490479-623df8f1-1a29-4d77-8c43-c305fffb99e7.png">
<img width="471" alt="image" src="https://user-images.githubusercontent.com/19230800/196490551-c76671b5-e08d-489b-a52b-881e26b651e6.png">
<img width="474" alt="image" src="https://user-images.githubusercontent.com/19230800/196490583-ffc1e21f-9a6d-49de-8384-3ec2ea9f2edc.png">
<img width="471" alt="image" src="https://user-images.githubusercontent.com/19230800/196490630-16c50899-cbd8-4778-8c94-913724f0e383.png">
<img width="481" alt="image" src="https://user-images.githubusercontent.com/19230800/196490668-7a053c12-daf0-41db-9d94-8751b3cf7a66.png">

---
License
----

- Apache 2.0
                              
Contributors
----

  - IBM SPSS
