;****************************************************************************************
;Products Counter
; This code counts the products that pass by 2 sensors,and depending on the direction of 
;entering, the number of the products will change
;Connections:
;		Inputs:
;			switch : RB0
;			sensor 2 : RA2
;			sensor 1 : RA3
;		Outputs:
;			7-Segment A-G: PORTD 0-6 
;			Enable for LSD 7-Segment : RA0
;			Enable for MSD 7-Segment : RA1
;			Buzzer : RB1
;			LED : RB5
;hardware request:SW S6 set ON, S5 the first and second set ON,S1 ON ,S12 and S13 OFF
;__CONFIG _DEBUG_OFF&_CP_OFF&_WRT_HALF&_CPD_OFF&_LVP_OFF&_BODEN_OFF&_PWRTE_OFF&_WDT_OFF&_XT_OSC
;****************************************************************************************	
INCLUDE "P16F877A.INC"
;****************************************************************************************
	CBLOCK	0X20
		Counter_LOWD			;holds the digit to be displayed on LSD 7_segment
		Counter_HIGHD			;holds the digit to be displayed on MSD 7_segment
		TIMER_LED				;holds the number of times the delay will be repeated to flash
								;the LED when the number of products is out of bound
		cmin_l					;holds the LSD of Cmin
		cmin_h					;holds the MSD of Cmin
		cmax_l					;holds the LSD of Cmax
		cmax_h					;holds the MSD of Cmax
		TIMER_COUNTER			;holds the number of times the delay will be repeated to turn
								; on/off the LED
    	TIMER_7seg				;holds the number of times the delay will be repeated to change
								;the state of 7-seg displays
		Temp_CH					;holds the content of Counter_HIGHD while ISR is running
		Temp_CL					;holds the content of Counter_LOWD while ISR is running
	ENDC
;****************************************************************************************
	ORG	0X000
	GOTO	MAIN
	ORG	0X004
	GOTO	ISR
;****************************************************************************************
MAIN
         	CALL	INITIAL
			BANKSEL 0X20
			MOVLW   .2
			MOVWF   cmax_h
			movlw   .5
			movwf   cmax_l		;Cmax=25
			movlw   .5
			movwf   cmin_l
			clrf    cmin_h		;Cmin=5
MAIN_LOOP	MOVLW   .3
			MOVWF   TIMER_LED   ;to display out of bound T=3 sec
			MOVLW   .10
			MOVWF	TIMER_COUNTER ;to let the led turn on/off 0.5 sec
			MOVLW   .10
			MOVWF	TIMER_7seg ;to show the number of products on 7-seg displays for 2 sec

   			GOTO   	Check_Sensors

;***********************************INITIAL*****************************************************
INITIAL
	BANKSEL	TRISA 				      
 	movlw   0X0C
	movwf   TRISA    			;TRISA,RA2,RA3 INPUT FOR SENSORS
	CLRF	TRISD				;TRISD FOR 7 SEG-DISPLAY
	CLRF	TRISB				;RB1 is for buzzer,and RB5 is for LED
	BSF     TRISB,0  			;RB0 is external interrupt for pause switch 						

	BANKSEL PORTA
	CLRF    PORTA
	CLRF    PORTD
	CLRF    PORTB
	CLRF	Counter_LOWD			
	CLRF	Counter_HIGHD		;Initially, the number to be displayed is 00
    CLRF	TIMER_7seg
  	BANKSEL INTCON
    BSF		INTCON, GIE			;Interrupts are Enabled
	BSF		INTCON, INTE		;External Interrupt is Enabled
	BCF		INTCON, INTF
	BANKSEL ADCON1
	MOVLW   06H                                                    
 	MOVWF   ADCON1               ;set PORTA as general Digital I/O PORT  
	RETURN

;***********************************ISR CODE*****************************************************
ISR 
PASS
	BANKSEL 0X20
	clrf     Temp_CL
	movf     Counter_LOWD,w
	movwf    Temp_CL			;save value of Counter_LOWD in temporary Register

    clrf     Temp_CH
	movf     Counter_HIGHD,w
	movwf    Temp_CH     	    ;save value of Counter_HIGHD in temporary Register

	BCF     PORTA,0	            ;disable MSD 7_segment display  
	BCF     PORTA,1             ;disable LSD 7_segment display  
	

	BANKSEL PORTB
	BTFSS	PORTB,0				;External Interrupt 
	GOTO    PASS
	BANKSEL INTCON
	BCF     INTCON,INTF

    BANKSEL 0X20
	clrf     Counter_LOWD
	movf     Temp_CL,0
	movwf    Counter_LOWD		;return value of Counter_LOWD
	

	clrf     Counter_HIGHD
	movf     Temp_CH,0
	movwf    Counter_HIGHD      ;return value of Counter_HIGHD

	RETFIE

;***********************************Check Sensors*****************************************************
Check_Sensors
right_to_left_case
	BANKSEL PORTA
	CALL  DELAY
	btfsc PORTA,2      		    ;check sensor2
	goto check_sensor_1         ;sensor2 occured
	goto left_to_right_case     ;no signal in sensor 2 goto case2

check_sensor_1
	call  DELAY
	btfsc PORTA,3 			   ;check sensor1
	goto increment  		   ;sensor1 and sensor2 occured then go to increment count
	goto MAIN_LOOP   		   ;there is no object has passed from right to left

left_to_right_case  
	call  DELAY
	btfsc PORTA,3 			   ;check sensor1
    goto check_sensor_2        ;sensor1 occured then goto check sensor2
    goto MAIN_LOOP             ;no signal in sensor 1 goto MAIN_LOOP

check_sensor_2   
	call  DELAY   
	btfsc PORTA,2 				;check sensor2
    goto decrement 				;sensor1 and sensor 2 occured then go to decrement count
    goto MAIN_LOOP				;there is no object has passed from left to right

;***********************************increment*****************************************************
increment            
	 movf Counter_LOWD ,0 		;w=low digit
     sublw .9 					;9-lowdigit
     btfsc  STATUS,Z   
     goto UPDATE_HIGH_DIGIT 	;if it is zero increment high digit and clrf low digit
     goto UPDATE_LOW_DIGIT 		;if not zero increment low digit

;***********************************UPDATE DIGITS*****************************************************
UPDATE_LOW_DIGIT    
	 INCF  Counter_LOWD,1
     goto check_bounds			;checking if the number of products is between Cmax and Cmin

UPDATE_HIGH_DIGIT    
	clrf  Counter_LOWD   
    incf  Counter_HIGHD ,1 
    goto   check_bounds			;checking if the number of products is between Cmax and Cmin

;***********************************decrement*****************************************************
decrement        
    movf Counter_LOWD ,1     
    btfss STATUS,Z  			;test if low digit=0
    goto decrement_low 			;if it is not zero decrement low 
    goto decrement_high  		;if it is zero decrement high and make low =9

decrement_low       
    decf Counter_LOWD,1
    goto check_bounds

decrement_high   
	 movlw .9 
     movwf Counter_LOWD   
     decf  Counter_HIGHD  
     goto  check_bounds

;***********************************Check BOUNDS*****************************************************
check_bounds
check_cmaxh 						;checking the MSD of Cmax
     movf Counter_HIGHD,0
	 subwf cmax_h,0  		        ;  2 - Counter_HIGHD
	 btfsc STATUS,Z			    	;check if Counter_HIGHD is 2 or not 	
	 goto check_cmax_l	        	; if Counter_HIGHD is 2 then check cmax_l
     btfsc STATUS,C		        	;if Counter_HIGHD is not equal to 2
                                    ;check if Counter_HIGHD is larger than 2 or not
	 goto check_cmin                ; if Counter_HIGHD is smaller than 2 then
									; we need to check if it larger than the min limit or not
	 goto DisplayLED			    ;if Counter_HIGHD is larger than 2 then it is out of bound

check_cmax_l						;checking the LSD of Cmax
	 movf Counter_LOWD,0
	 subwf cmax_l ,0				;5 - Counter_LOWD
	 btfsc STATUS,C					;if C =1 then we are less than the max value
	 goto check_cmin			
	 goto DisplayLED				;if Counter_LOWD is larger than 5 then we are out of bound

check_cmin 
	 movf  Counter_HIGHD,1
	 btfss STATUS,Z					;check if Counter_HIGHD is 0 or not 
	 goto DisplayCounter			;if is not 0 then just display it because we are between Cmax and Cmin
	 movf Counter_LOWD,0			;if is 0 we need to check if Counter_LOWD is smaller than 5
	 subwf cmin_l,0
     btfsc STATUS,C			
	 goto DisplayLED				;if there is carry then 5 is larger than Counter_LOWD 
									;and we are out of bound
     goto DisplayCounter			;if C=0 then Counter_LOWD is larger than 5 and we are between 25 and 5 
									;then display

;***********************************DisplayLED*****************************************************
DisplayLED   						;displaying if out of bound

	BANKSEL	PORTB 
    Bsf     PORTB,1   				; Buzzer is on      

LED_FLASH  
	MOVLW   .10						;we need to let 50m sec being repeated 10 times so get 0.5 sec
    MOVWF	TIMER_COUNTER 			;so the LED is on for 0.5 sec                 
LED_ON    
	Bsf     PORTB,5             	; LED is on                                 
 	CALL    DELAY	  
	decfsz  TIMER_COUNTER,1    
    goto    LED_ON

    MOVLW   .10						;we need to let 50m sec being repeated 10 times so get 0.5 sec
	MOVWF	TIMER_COUNTER 			;so the LED is off for 0.5 sec
LED_OFF                                    
    Bcf     PORTB,5 
	CALL    DELAY            		; LED is off 
    decfsz  TIMER_COUNTER,1    
    goto    LED_OFF

	decfsz  TIMER_LED,1    			;we need to repeat the previous code 3 times to flash
    goto    LED_FLASH				;LED for 3 sec
    

	Bcf     PORTB,1      			;Buzzer is off
    GOTO    MAIN_LOOP

;***********************************DisplayCounter*****************************************************
DisplayCounter						;showing the number of products
									;displaying 2 sec for each number 
REPEAT  
	MOVF   	Counter_LOWD, W         ;displaying LSD                             
 	CALL    Look_TABLE                                                 
 	MOVWF   PORTD                                                  
 	BSF     PORTA,0                 ;enable LSD 7_segment display                               
 	CALL    DELAY 
	CALL    DELAY  	                                                  
 	BCF     PORTA,0     			;disable LSD 7_segment display 
                                                                                                     
 	MOVF   	Counter_HIGHD, W        ;displaying MSD           
 	CALL    Look_TABLE
	MOVWF   PORTD
 	BSF     PORTA,1					;enable MSD 7_segment display 
	CALL    DELAY
	CALL    DELAY	                                                  
 	BCF     PORTA,1 				;disable MSD 7_segment display 
	
	DECFSZ  TIMER_7seg,1  
	GOTO    REPEAT
	MOVLW   .10
	MOVWF	TIMER_7seg 

	BANKSEL	PORTB 
    BSF     PORTB,1      		    ; Buzzer is on 
repeat_buzzer
    CALL    DELAY
	DECFSZ  TIMER_7seg,1
	GOTO 	repeat_buzzer	
	BCF     PORTB,1                 ;Buzzer is off after 10 rounds which means after 0.5 sec
    GOTO    MAIN_LOOP
;*****************************LOOKUP_TABLE***********************************************************	
Look_TABLE 
	ADDWF  	 PCL,	1
	RETLW	B'11000000'		;'0'
	RETLW	B'11111001'		;'1'	
	RETLW	B'10100100'		;'2' 		
	RETLW	B'10110000'		;'3' 		
	RETLW	B'10011001'		;'4'
	RETLW	B'10010010'		;'5'		
	RETLW	B'10000010' 	;'6'
	RETLW	B'11111000'		;'7'		
	RETLW	B'10000000'		;'8'
	RETLW	B'10010000'		;'9'
;****************************************DELAY******************************************************
DELAY  ;50msec	
		BANKSEL	TMR0
		movlw 	D'61' 		  ;256-195=61,N=195
		movwf 	TMR0
		BANKSEL	OPTION_REG
		movlw 	B'00000111' 	;set up T0 for internal clock, prescale by 256
		movwf 	OPTION_REG
		BANKSEL	TMR0
L1	 	btfss 	INTCON,T0IF 	;test for Timer Overflow flag
		goto  	L1	    		;loop if not set (no timer overflow)
		bcf   	INTCON,T0IF 	;clear Timer Overflow flag
		return
end