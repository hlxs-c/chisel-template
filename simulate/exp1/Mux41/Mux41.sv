module Mux41(a, s, y);
    input [3:0] a;  // define a wire type variable, the width of a is 4 bit
    input [1:0] s;  // define a wire type variable ,the width of b is 2 bit
    output reg y;   // define a reg type variable, the width of y is 1 bit 

    always @(s or a)
        case (s)
            0: y = a[0];
            1: y = a[1];
            2: y = a[2];
            3: y = a[3];
            default: y = 1'b0;
        endcase
endmodule
