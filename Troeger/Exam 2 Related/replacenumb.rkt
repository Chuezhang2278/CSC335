; Sample input 1122 sample outout 2222
; Development: I can maybe go through each digit one at a time and see if it is equal to the input value and if so, replace it
;            : The qeustion is how do i replace it because its not a list, i can't just say i want to replace this digit
;            : Perhaps what i can do is add to a total for every digit processed and if the digit im processing is equal to the value i want to swap
;            : Then i change it
;            : I.E 12345 1 5
;            : 12345 0
;            : 1234 5
;            : 123 45
;            : 12 345
;            : 1 2345
;            : 1 is to be swapped by a 5
;            : 0 52345
;What i need: A way to go through digits considering the digit placement
;                   - I will be going back right to left so perhaps i can setup a count to accomodate for the digit count
;             A method to check if the current digit is equal to the digit we want to swap
s