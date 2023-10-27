Durata della stagione
================
Dr. Raffaele Morelli
27 ottobre, 2023 - 11:39

    ## 
    ## 
    ## ##  ITC43 Lecco
    ## 
    ## 
    ## Call:
    ## lm(formula = durata ~ quota + anno, data = df)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -106.890  -19.762    1.924   21.930  107.398 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 1884.76444  157.56806  11.962  < 2e-16 ***
    ## quota700       0.42593    6.29800   0.068   0.9461    
    ## quota800       0.03704    6.29800   0.006   0.9953    
    ## quota900      -1.59259    6.29800  -0.253   0.8004    
    ## quota1000     14.42593    6.29800   2.291   0.0223 *  
    ## quota1100     37.37037    6.29800   5.934 4.69e-09 ***
    ## quota1200     32.22222    6.29800   5.116 4.05e-07 ***
    ## quota1300     62.57407    6.29800   9.936  < 2e-16 ***
    ## quota1400     59.37037    6.29800   9.427  < 2e-16 ***
    ## quota1500     70.57407    6.29800  11.206  < 2e-16 ***
    ## quota1600     70.57407    6.29800  11.206  < 2e-16 ***
    ## quota1700     98.98148    6.29800  15.716  < 2e-16 ***
    ## quota1800     96.20370    6.29800  15.275  < 2e-16 ***
    ## anno          -0.94084    0.07925 -11.872  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 32.73 on 688 degrees of freedom
    ## Multiple R-squared:  0.5867, Adjusted R-squared:  0.5789 
    ## F-statistic: 75.12 on 13 and 688 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## 
    ## ##  ITC41 Varese
    ## 
    ## 
    ## Call:
    ## lm(formula = durata ~ quota + anno, data = df)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -57.278 -26.234  -8.475  26.874  88.517 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 1162.1902   252.2226   4.608 6.34e-06 ***
    ## quota700      -1.2222     6.2537  -0.195 0.845200    
    ## quota800       8.7963     6.2537   1.407 0.160731    
    ## quota900      25.7407     6.2537   4.116 5.16e-05 ***
    ## quota1000     22.8148     6.2537   3.648 0.000318 ***
    ## anno          -0.5745     0.1269  -4.527 9.05e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 32.5 on 264 degrees of freedom
    ## Multiple R-squared:  0.1667, Adjusted R-squared:  0.1509 
    ## F-statistic: 10.56 on 5 and 264 DF,  p-value: 2.929e-09
