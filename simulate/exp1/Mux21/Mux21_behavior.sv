module Mux21(a, b, s, y);
    input a, b, s;
    output reg y;   // y is assign in always block, so y is must be reg type

    always @(*)
        if(s == 0)
            y = a;
        else
            y = b;
endmodule