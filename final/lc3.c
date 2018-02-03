/*
 * CS 350 LC3 Final Project
 */

#include <stdio.h>
#include <stdlib.h>       // For error exit()
#include <string.h>       // for strcmp
#include "lc3.h"
#include "math.h"

/*
 * Explains the usage of this program
 */


// 8 bit floating point funtionality

static float
flt_fun(int m, int e){
    // m is 1001 0111    e is 5
    float x = 0;
    int i = 0;
    while(i!=12){
        int bit = ((m >> (11-i)) & 1); // pull first bit off left of 8 bit number
        x = x + bit * pow(2,(e-i));
        i++;
    }
    return x;
}


static void
flt_fun_base(cpu_t *cpu, uint8_t dr) {
    /* ----------------------------------New floating point function
     *  This function takes what is stored in a register, casts it to the flt structure
     *  then processes the mantissa and exp and sign to return a floating point value.
     *
    */
    
        flt_t *f =(flt_t*) cpu->freg[dr];
        short int exp = f->exp;
        uint16_t mant = f->mant;
        uint16_t m = mant;
    
        // First we deal with the sign bit
        int sign = 1;
        if (f->sign == 1){
            sign = -1;
        }
    
        // Next I process out the special cases
    
        if (exp == 0) { // Denormalized no implied 1 bit for mant
            exp = -6;
            float y = flt_fun(mant, exp);
            printf("Mantissa = x%02hx; Exp = %d Float is %f\n", m, exp, y);
        }
    
        else if (exp == 15 & mant == 0){ // infinity
            printf("Infinity\n");
        }
    
        else if (exp == 15 & mant > 0){ //  Not a number
            printf("NaN\n");
        }
    
        else{ // Normal case
            exp = exp - 7; // This adjusts for the exp bias
            mant = mant|1<<11; // Adding the assumed bit to the mantissa at bit 12
            float y = flt_fun(mant, exp);
            printf("Mantissa = x%02hx; Exp = %d Float is %f\n", m, exp, y);
        }
    return;
}


/*
 *  The purpose of this function is to process the mantissa and the exponent 
 *  and return it to the add function
 *
*/


void
flt_process(short int* exp, uint16_t* mant){
    
    if (*exp == 0) { // Denormalized no implied 1 bit for mant
        *exp = -6;
    }
    
    else{ // Normal case
        *exp = *exp - 7; // This adjusts for the exp bias
        *mant = *mant|1<<11; // Adding the assumed bit to the mantissa at bit 12
        //float y = flt_fun(mant, exp);
        //printf("Mantissa = x%02hx; Exp = %d Float is %f\n", m, exp, y);
    }

    
}

 
/*
 *  This is the float add function. it takes to floating point values and adds them together
 *
 *
 *
 */

static void
flt_add(cpu_t *cpu, uint8_t dr, uint8_t fr1, uint8_t fr2) {
    flt_t *f1 =(flt_t*) cpu->freg[fr1];
    flt_t *f2 =(flt_t*) cpu->freg[fr2];
    
    // don't forget to adjust for the sign
    
    // Declare variables
    short int exp1 = f1->exp;
    short int exp2 = f2->exp;
    uint16_t man1 = f1->mant;
    uint16_t man2 = f2->mant;
    uint8_t s1 = f1->sign;
    uint8_t s2 = f2->sign;
    
    // First process out infinity and not a number
    if ((exp1 == 15 & man1 == 0 ) || (exp2 == 15 & man2 == 0)){
        printf("Infinity\n");
        return;
    }
    
    else if ((exp1 == 15 & man1 > 0) || (exp2 == 15 & man2 > 0)){ //  Not a number
        printf("NaN\n");
        return;
    }
    // Next process the denormalized and normalized case
    flt_process(&exp1, &man1);
    flt_process(&exp2, &man2);
    
    // At this point we have our actual exponent value and our actual mantissa
    // Now we need to adjust the mantissa and the exponent
    short int addexp = 0;
    switch ((int) (exp2 > exp1 && exp1 != exp2)) {
        case 0:{
            addexp = exp1 - exp2;
            exp2 = addexp + exp2;
            man2 = man2 >> addexp; // Adjusting the radix
            break;
            }
        case 1:{
            addexp = exp2 - exp1;
            exp1 = addexp + exp1;
            man1 = man1 >> addexp; // adjusting the radix
            break;
        }
        default:{
            break;
        }
    }
    // Finally we can begin to math out our problem
    
    uint16_t man;
    if(s1^s2){
        // negative
        switch(s1){
            case 0:{
                // s1 is positive s2 is negative. So two's comp that shit to subtract
                man = man1+((~man2)+1);
                
            }
            case 1:{
                // s2 is positive s1 is negative. So two's comp that shit to subtract
                man = man2+((~man1)+1);
            }
        }
    }
    else{
        // normal addition
        man = man1+man2;
    }
}



static int sext(int16_t x, int16_t b){
    return (((x) << (16-b)) >> (16-b));
}

static void
usage (char *argv[])
{
    printf("Usage: %s <.hex file>\n", argv[0]);
    exit(0);
}


// FILL ME IN
static int
read_hex_number (FILE *datafile, unsigned int *value_read)
{
    
#define HEX_BUFFER_LEN 80
    char hex_buffer[HEX_BUFFER_LEN];	// Text of current line
    int words_read=0;						// Nbr words read f/buffer
    char *read_success; 				// NULL if reading in a line fails
    int found_nbr = 0;					// true if we have read a hex nbr
    
    // FILL ME IN: Read a line from the data file and read a hex value from
    // the line.  Return true (1) if we succeed; return false (0) if the
    // data file read or hex value read failed.
    read_success = fgets(hex_buffer, HEX_BUFFER_LEN, datafile);
    while(read_success != NULL && found_nbr == 0){
        words_read = sscanf(hex_buffer, "%x", value_read);  // I think value read is correct.
        if (words_read>=1){
            return 1;
        }
        read_success = fgets(hex_buffer, HEX_BUFFER_LEN, datafile);
    }
    
    
    // Note if the line of input begins with an integer, treat
    // it as the memory value to read in.  Ignore junk
    // after the number and ignore blank lines and lines
    // that don't begin with a number.
    
    return 0;
}



/* -------------------- CONDITION CODE ROUTINES --------------------
 *
 * Return character N, Z, or P depending on cpu condition code.
 * Return '?' for a bad condition code.
 */
static const char
cc2char (cpu_t *cpu)
{
    cc_t cc = cpu->cc;
    if (cc == CC_NEG) {
        return 'N';
    } else if (cc == CC_ZER) {
        return 'Z';
    } else if (cc == CC_POS) {
        return 'P';
    } else {
        return '?';
    }
}


/*
 * Set cc to 4 if value is negative, 2 if zero, 1 if positive.
 *
 * FILL ME IN
 *
 */
static void
set_cc (cpu_t *cpu, word_t value, instr_t *instr)
{
    
    // XFILL ME IN: set the condition codes based on the value
    // passed in. See the definitions of CC_NEG, etc. in lc3.h
    
    // I think i did this right I may need to use actual number not CC_NEG
    if (value<0){
        cpu->cc = CC_NEG;
    }
    if (value == 0){
        cpu->cc = CC_ZER;
    }
    if (value > 0){
        cpu->cc = CC_POS;
    }
    
    printf("CC = %c", cc2char(cpu));
    
    // Add trailing newline except for trap instructions
    if (instr->opcode != 0xF) {
        printf("\n");
    }
}


/*_
 * field(value, hi, lo) returns bits value[hi:lo], zero-filled.
 * (Bits are numbered 15..0, left to right.)
 */
//static word_t
//field (word_t value, int high, int low)
//{   // this makes no sense
//    // Say we want to select n = high-low+1 bits.
//    // Then (value >> low) right-justifies those n bits,
//    // and (0xffff << n) is a mask with 16-n 1's and n 0's
//    // so ~(0xffff << n) is a mask with 16-n 0's and n 1's.
//    //
//    //return (value >> low) & ~((Word) 0xffff << (high-low+1));
//    word_t mask = (1 << (high-low+1)) - 1;
//    return (value >> low) & mask;
//}

/*
 * sign_ext(value, hi, lo) returns bits value[hi:lo], sign-filled.
 * (Bits are numbered 15..0, left to right.)
 *
 * FILL ME IN
 *
 */
static word_t
sign_ext (word_t value, int high, int low)
{
    high=high+1;
    // FILL ME IN: take the index 'high' as the bit
    // position that contains the sign bit. Drag this
    // bit across appropriately to sign extend the number
    // to a full 16 bits.
    return (((value) << (16-high)) >> (16-high));
}


/* -------------------- PRINT INSTRUCTION ROUTINES -------------------- */




/*
 * Format for printing instructions
 *
 * Opcodes are 5 chars, left-justified; there's a space and then a sequence
 * of registers and/or values, separated by comma space. Registers are R0 -- R7.
 * Decimal values are left-justified with a leading hyphen (for negative values)
 *     or a leading space (for non-negative values)
 * Hex values are printed as 2 hex digits with preceding x
 * Trailing spaces are printed out so that every instruction takes 18 characters.
 
 * print_op(op), print_val(op, val) and print_hexval(op, val)
 * prints the opcode and (if given) the value in decimal or hex.
 */
static void
print_op (char *op)
{
    printf("%-5s %13s", op, "");
}

static void
print_val (char *op, word_t val)
{
    printf("%-5s %-6d %6s", op, val, "");
}

static void
print_hexval (char *op, word_t val)
{
    printf("%-5s x%-2hX %9s", op, val, "");
}

static void
print_fltval (char *op, uint8_t man, int exp)
{
    printf("%-5s x%hhx %d %9s", op, man, exp, "");
}

/*
 * print_reg_val(op, reg, val) and print_2reg_val(op, r1, r2, val)
 * prints out the opcode, register(s), and value
 */
static void
print_reg_val (char *op, reg_t reg, word_t val)
{
    printf("%-5s R%d, %-5d %3s", op, reg, val,"");
}

static void
print_2reg_val (char *op, reg_t r1, reg_t r2, word_t val)
{
    printf("%-5s R%d, R%d, %-4d ", op, r1, r2, val);
}

/*
 * print_reg(op, reg), print_2reg(op, r1, r2), and print_3reg(op, r1, r2, r3)
 * prints out the opcode and register(s)
 */
static void
print_reg (char *op, reg_t reg)
{
    printf("%-5s R%d %10s", op, reg, "");
}

static void
print_2reg (char *op, reg_t r1, reg_t r2)
{
    printf("%-5s R%d, R%d %6s", op, r1, r2, "");
}

static void
print_3reg (char *op, reg_t r1, reg_t r2, reg_t r3)
{
    printf("%-5s R%d, R%d, R%d %2s", op, r1, r2, r3, "");
}


/*
 * print_instr(instr) prints a word as an assembler instruction, with a
 * mnemonic and some number of register or value arguments.
 * An 18-char string is printed (with no trailing '\n').
 */
void
print_instr (instr_t *instr)
{
    char *op_mnemonic[16] = {"BR","ADD","LD","ST","JSR","AND","LDR","STR",
        "RTI","NOT","LDI","STI","JMP","FLT","LEA","TRAP"};
    int op_field = instr->opcode;
    char *op = op_mnemonic[op_field];
    char *br_mnemonic[] = {"NOP","BRP","BRZ","BRZP","BRN","BRNP","BRNZ","BR"};
    
    switch (op_field) {
        case 0: // BR
        {
            br_t * b = (br_t*)instr;
            int cc_field = (b->val >> 9) & 0x7;
            print_val(br_mnemonic[cc_field], sign_ext(b->pcoffset9,8,0));
            break;
        }
        case 1:
        case 5: // ADD, AND
        {
            add_and_reg_t* a = (add_and_reg_t*)instr;
            
            if (a->flag == 0) { // 0 means reg
                print_3reg(op, a->dr, a->sr1, a->sr2);
            } else { // 1 means immediate
                add_and_imm_t * addi = (add_and_imm_t*)instr;
                print_2reg_val(op, addi->dr, addi->sr1, sign_ext(addi->imm5,4,0));
            }
            break;
        }
        case 2:
        case 3:
        case 10:
        case 11:
        case 14: // LD, ST, LDI, STI, LEA
        {
            ld_ldi_lea_t *l = (ld_ldi_lea_t*)instr;
            print_reg_val(op, l->dr, sign_ext(l->pcoffset9, 8, 0));
            break;
        }
        case 4: // JSR/JSRR
        {
            jsr_t * jsr = (jsr_t*)instr;
            
            if (jsr->flag == 0) {
                jsrr_t * jsrr = (jsrr_t*)instr;
                print_reg("JSRR", jsrr->baser);
            } else {
                print_val("JSR", sign_ext(jsr->pcoffset11, 10, 0));
            }
            break;
        }
        case 6:
        case 7: // LDR, STR
        {
            ldr_t * l = (ldr_t*)instr;
            print_2reg_val(op, l->dr, l->baser, sign_ext(l->offset6, 5, 0));
            break;
        }
        case 8:
        case 13: // RTI, ERR
        {
            flt_t *f =(flt_t*) instr;
            
            printf("%-5s x%04hhx %d %9s", op, f->mant, f->exp, "");
            break;
        }
        case 9: // NOT
        {
            not_t * n = (not_t*)instr;
            print_2reg(op, n->dr, n->sr);
            break;
        }
        case 12: // JMP
        {
            jmp_ret_t * j = (jmp_ret_t*)instr;
            print_reg(op, j->baser);
            break;
        }
        case 15: // TRAP
        {
            trap_t *t = (trap_t*)instr;
            print_hexval(op, t->trapvect8);
            break;
        }
        default:
            break;
    }
}


/*
 * print_addr_val_hex_instr(addr, val) prints
 * out the address in hex colon, value in decimal,
 * and value as an instruction.
 */
static void
print_addr_val_hex_instr (address_t addr, instr_t *instr)
{
    printf("x%04hX: x%04hX  ", addr, instr->val);
    print_instr(instr);
}


/*
 * print_addr_val_hex_dec_instr(addr, val) prints
 * out the address in hex colon, value in hex, value in decimal,
 * and value as an instruction.
 */
static void
print_addr_val_hex_dec_instr (address_t addr, instr_t *instr)
{
    if (instr->val != 0) {
        printf("x%04hX: x%04hX  % 6d  ", addr, instr->val, instr->val);
        print_instr(instr);
        printf("\n");
    }
}




/* -------------------- SIMULATOR ROUTINES -------------------- */


/*
 * Print standard message for simulator help command ('h' or '?')
 */
static void
help_msg (void)
{
    printf("Simulator commands:\n");
    printf("h, H, or ? for help (prints this message)\n");
    printf("q or Q to quit\n");
    printf("d or D to dump the control unit and memory\n");
    printf("An integer > 0 to execute that many instruction cycles\n");
    printf("Or just return, which executes one instruction cycle\n");
}


/*
 * This function will look at the IR and will fill in the
 * values of an instruction struct
 */
static void
decode_instr (word_t ir, instr_t *instr)
{
    // Note that we don't really need to do any decoding here as
    // that will be taken care of by casting the instruction
    // to different types of structs (see lc3.h and explanations)
    instr->val = ir;
}


/* -------------------- LC-3 INSTRUCTION ROUTINES --------------------
 *
 * Execute branch instruction: Bitwise AND instruction's mask and
 * cpu's condition code, branch if result is nonzero.
 *
 * Echo kind of branch, current cc, whether or not the branch happened,
 * and if so, the target of the branch.
 *
 * NOTE: branch does NOT modify condition codes
 *
 * FILL ME IN: finish this
 *----------------------------DONE
 */
static void
instr_br (cpu_t *cpu, instr_t *instr)
{
    br_t *branch_instr = (br_t*)instr;
    cc_t cc = 0;
    
    // FILL ME IN: extract cc from the decoded branch instruction,
    // it should not just be 0!
    if (branch_instr->n == 1) {
        cc+=CC_NEG;
    }
    if (branch_instr->p == 1) {
        cc+=CC_POS;
    }
    if (branch_instr->z == 1) {
        cc+=CC_ZER;
    }
    
    printf("; CC = %c, ", cc2char(cpu));
    
    if ((cc & cpu->cc) == 0) {
        printf("no branch\n");
    } else {
        // FILL ME IN: compute the target correctly
        short x = sext(branch_instr->pcoffset9, 9);
        printf("PC <- x%hX%+d = x%hX\n", cpu->pc, branch_instr->pcoffset9, (unsigned short) (cpu->pc+x));
        cpu->pc = (cpu->pc+x);
    }
    return;
}
/* this is my implementation of the float function
 *
 */




/*
 * Execute add instruction: Destination register = left operand + right
 * operand.  Left operand is src1 register, right operand is either src2
 * register or an immediate field (sign-filled).
 *
 * Echo type of add, sources/values of operand, destination/value of
 * result.
 *
 * NOTE: add DOES modify condition codes
 *
 * FILL ME IN
 * -------------------------------------DONE with exception of the incremented pc thing
 */
static void
instr_add (cpu_t *cpu, instr_t *instr)
{
    int x;
    add_and_reg_t *add_instr = (add_and_reg_t*)instr;
    
    // FILL ME IN: finish this function, setting the result, and
    // setting the condition codes properly. Note that this should
    // handle both add and add immediate. Note the check for the flag
    // in the decoded instruction to see which one it is. You may need
    // to cast instr as a add_and_imm_t*! You'll have to change the
    // arguments to the printfs!
    
    if (add_instr->flag == 1) {
        // FILL ME IN  still need to sign extend
        add_and_imm_t *add_instri =(add_and_imm_t*)add_instr;
        
        x = sign_ext(add_instri->imm5, 4, 0);
        printf("; R%d <- x%hX %+d = x%hX; ", add_instri->dr, (unsigned short) cpu->reg[add_instri->sr1], x, (unsigned short) (cpu->reg[add_instri->sr1] + x));
        x = cpu->reg[add_instri->sr1] + x;
        }
    else {
        // FILL ME IN
        x = cpu->reg[add_instr->sr1] + cpu->reg[add_instr->sr2];
        printf("; R%d <- x%hX + x%hX = x%hX; ", add_instr->dr, (unsigned short) cpu->reg[add_instr->sr1], (unsigned short) cpu->reg[add_instr->sr2], (unsigned short) x);
        }
    // FILL ME IN
    
    //Add to the dr
    cpu->reg[add_instr->dr] = x;
    //Set the cc
    set_cc(cpu, x, instr);

}

static void
instr_flt (cpu_t *cpu, instr_t *instr){
    flt_case_t *f =(flt_case_t*) instr;
    
    switch (f->flag) {  // 0 is ld;   1 is st;    2 is add;    3 is display.
        case 0:{
            // This is the load case, FReg[fr] <- M[PC+Offset8]
            flt_ls_t *fs =(flt_ls_t*) instr;
            int x = sext(fs->pcoffset8 , 8);
            printf("; FR%d <- M[x%04hX] = x%hX; ", fs->fr, (unsigned short) (x+cpu->pc), cpu->mem[cpu->pc+x]);
            cpu->reg[f->dr]=cpu->mem[(cpu->pc)+x];
            set_cc(cpu ,cpu->reg[fs->fr], instr);
        }
        case 1:{
            // This is the ST case M[PC+offset] <- FReg[fr]
            flt_ls_t *fs =(flt_ls_t*) instr;
            short x = sext(fs->pcoffset8 , 8);
            printf("; M[x%04hX] <- x%hX\n", (unsigned short) (x+cpu->pc), (unsigned short) cpu->reg[fs->fr]);
            cpu->mem[x+cpu->pc] = cpu->reg[fs->fr];
        }
        case 2:{
            // This will be the add function, so FReg[dr] <- FReg[fr1] + FReg[fr2]
            flt_case_t *fs =(flt_case_t*) instr;
            flt_add(cpu, fs->dr, fs->fr1, fs->fr2);
        }
        case 3:{
            // This is the display function which just displays the float value stored in a register
            flt_fun_base(cpu, f->dr);
        }
            
    }
    
    return;
    
}

// ----------------------------------Orignal floating point instructions
//    flt_t *f =(flt_t*) instr;
//    short int exp = f->exp;
//    uint16_t mant = f->mant;
//    uint16_t m = mant;
//    // deal with sign bit
//    int sign = 1;
//    if (f->sign == 1){
//        sign = -1;
//    }
//
//    if (exp == 0) { // denormalized no implied 1 bit for mant
//        exp = -6;
//        float y = flt_fun(mant, exp);
//        printf("Mantissa = x%02hx; Exp = %d Float is %f\n", m, exp, y);
//    }
//    else if(exp == 15 & mant == 0){ // infinity
//        printf("Infinity\n");
//    }
//    else if(exp == 15 & mant > 0){ //  Not a number
//        printf("NaN\n");
//    }
//    else{ // normal case
//        exp = exp - 7; // exp bias
//        
//        
//        mant = mant|1<<7; // Adding the assumed bit to the mantissa
//        float y = flt_fun(mant, exp);
//        printf("Mantissa = x%02hx; Exp = %d Float is %f\n", m, exp, y);
//    }



               
        /*
         * Execute load instruction: Load destination register
         * from PC-offset memory location.  Set condition code.
         *
         * Echo destination, pc, offset, pc+offset, value loaded,
         * new condition code.
         *
         * NOTE: ld DOES modify condition codes
         *
         * FILL ME IN
         *-------------------------------------DONE though the cc thing is still throwing me off
         */
static void
instr_ld (cpu_t *cpu, instr_t *instr)
{
    // FILL ME IN
    ld_ldi_lea_t *l = (ld_ldi_lea_t*)instr;
    int x = sext(l->pcoffset9 , 9);
    printf("; R%d <- M[x%04hX] = x%hX; ", l->dr, (unsigned short) (x+cpu->pc), cpu->mem[cpu->pc+x]);
    cpu->reg[l->dr]=cpu->mem[(cpu->pc)+x];
    set_cc(cpu ,cpu->reg[l->dr], instr);

}


        /*
         * Execute store instruction: Store source register
         * to PC-offset memory location.  Set condition code.
         *
         * Echo source, pc, offset, pc+offset, value stored,
         * new condition code.
         *
         * NOTE: ST does not modify condition codes
         *
         * FILL ME IN
         * -------------------------------------DONE
         */
static void
instr_st (cpu_t *cpu, instr_t *instr)
{
    st_sti_t *s = (st_sti_t*)instr;
    
    short x = sext(s->pcoffset9 , 9);
    printf("; M[x%04hX] <- x%hX\n", (unsigned short) (x+cpu->pc), (unsigned short) cpu->reg[s->sr]);
    cpu->mem[x+cpu->pc] =cpu->reg[s->sr];
}


        /*
         * Execute jump subroutine instruction: Save R7 and branch
         * to target location (PC-offset or base register).
         *
         * NOTE: This handles both JSR and JSRR!
         *
         * Echo kind of JSR, target, and either PC & offset or
         * base register number.
         *
         * NOTE: jsr does NOT modify condition codes
         *
         * FILL ME IN
         *--------------------------------------------DONE I am pretty sure
         */
static void
instr_jsr (cpu_t *cpu, instr_t *instr)
{
    jsr_t *j = (jsr_t*)instr;
            
    if (j->flag == 1) { // this is JSR
                // FILL ME IN
        short x = sext(j->pcoffset11 , 11);
        printf("; PC <- x%hX %+d = x%hX, R7 <- x%hX\n", cpu->pc, x, (unsigned short) (cpu->pc+x), cpu->pc);
        cpu->reg[7] = cpu->pc;
        cpu->pc = cpu->pc+x;
    }
    else { // this is JSRR
        jsrr_t *jsrr = (jsrr_t*)instr;
        // FILL ME IN
        printf("; PC <- x%hX, R7 <- x%hX\n", cpu->reg[jsrr->baser], cpu->pc);
        cpu->reg[7] = cpu->pc;
        cpu->pc = cpu->reg[jsrr->baser];
    }
            
            // FILL ME IN
}
               
               
        /*
         * Execute and instruction: Destination register = left operand & right
         * operand.  Left operand is src1 register, right operand is either src2
         * register or an immediate field (sign-filled).  Set condition code.
         *
         * Echo type of add, sources/values of operand, destination/value of
         * result, new condition code.
         *
         * NOTE: and DOES modify condition codes
         *
         * FILL ME IN
         * ----------------------------------------------- DONE
         *  I left the non sext for imm5
         */
static void
instr_and (cpu_t *cpu, instr_t *instr)
{
    add_and_reg_t *a = (add_and_reg_t*)instr;

    if (a->flag == 1) {
        // FILL ME IN
        add_and_imm_t *aa = (add_and_imm_t*)instr;
        short x = sign_ext(aa->imm5, 4, 0);
        printf("; R%d <- x%04hX & x%04hX = x%hX; ", a->dr, cpu->reg[a->sr1], x, (unsigned short) (cpu->reg[a->sr1] & x));
        set_cc(cpu, cpu->reg[a->sr1] & x, instr);
        cpu->reg[a->dr] = cpu->reg[a->sr1] & x;
        
        
    }
    else {
        // FILL ME IN
        printf("; x%04hX & x%04hX = x%hX; ", cpu->reg[a->sr1], cpu->reg[a->sr2], (unsigned short) (cpu->reg[a->sr1] & cpu->reg[a->sr2]));
        set_cc(cpu, cpu->reg[a->sr1] & cpu->reg[a->sr2], instr);
        cpu->reg[a->dr] = cpu->reg[a->sr1] & cpu->reg[a->sr2];
    }
            
            // FILL ME IN
}
               
        /*
         * Execute load using base register: Load destination register
         * from base-offset memory location.  Set condition code.
         *
         * Echo destination, base, offset, base+offset, value loaded,
         * new condition code.
         *
         * NOTE: ldr DOES modify condition codes
         *
         * FILL ME IN
         * -----------------------------------------------DONE
         */
static void
instr_ldr (cpu_t *cpu, instr_t *instr)
{
    ldr_t *l = (ldr_t*)instr;
    // FILL ME IN
    short x = sext(l->offset6,6);
    printf("; R%d <- M[x%04hX %+d] = M[x%04hX] = x%hX; ", l->dr, (unsigned short) cpu->reg[l->baser], x, (unsigned short) (cpu->reg[l->baser]+x), cpu->mem[cpu->reg[l->baser]+x]);
    cpu->reg[l->dr] = cpu->mem[cpu->reg[l->baser]+x];
    // I'm guessing value on set_cc is whatever I am loading into the register
    set_cc(cpu, cpu->mem[cpu->reg[l->baser]+x], instr);
    
    
    // FILL ME IN
}
               

        /*
         * Execute store using base register: Store source register
         * to base-offset memory location.
         *
         * Echo source, base, offset, base+offset, value stored.
         *
         * NOTE: str does NOT modify condition codes
         *
         * FILL ME IN
         * ----------------------------------------------- DONE
         */
static void
instr_str (cpu_t *cpu, instr_t *instr)
{
    str_t *s = (str_t*)instr;
    // FILL ME IN
    int x = sext(s->offset6,6);
    printf("; M[x%04hX %+d] = M[x%04hX] <- x%hX\n", (unsigned short) cpu->reg[s->baser], x, (unsigned short) (cpu->reg[s->baser] + x), (unsigned short) cpu->reg[s->sr]);
    
    cpu->mem[cpu->reg[s->baser] + x] = cpu->reg[s->sr];
    
    // FILL ME IN
}
               
               
        /*
         * Return from interrupt command prints a message but continues execution.
         * THIS OPCODE IS IGNORED FOR OUR PURPOSES.
         * ----------------------------------------------- DONE
         */

static void
instr_rti (cpu_t *cpu, instr_t *instr)
{
    printf("; Opcode ignored\n");
}
               
               
        /*
         * Execute not instruction: Destination register = bitwise not
         * of source register.  Set condition code.
         *
         * Echo destination and source registers and values, result value,
         * new condition code.
         *
         * NOTE: not DOES modify condition codes
         *
         * FILL ME IN
         * ----------------------------------------------- DONE
         */
static void
instr_not (cpu_t *cpu, instr_t *instr)
{
    not_t *n = (not_t*)instr;
            
    // FILL ME IN
    
    printf("; R%d <- NOT x%04hX = x%04hX; ", n->dr, (unsigned short) cpu->reg[n->sr], (unsigned short) !(cpu->reg[n->sr]));
    set_cc(cpu, ~(cpu->reg[n->sr]), instr);
    cpu->reg[n->dr] = !(cpu->reg[n->sr]);
    
    
            
    // FILL ME IN
}

               
        /*
         * Execute load indirect: Load destination register from
         * indirect pc+offset memory location.  Set condition code.
         *
         * Echo destination, base, offset, base+offset, value at base+offset,
         * value loaded, new condition code.
         *
         * NOTE: ldi DOES modify condition codes
         *
         * FILL ME IN
         * ----------------------------------------------- DONE
         */
static void
instr_ldi (cpu_t *cpu, instr_t *instr)
{
    ld_ldi_lea_t *l = (ld_ldi_lea_t*)instr;
    
    // FILL ME IN
    int x = sext(l->pcoffset9,9);
    
    printf("; R%d <- M[M[x%04hX]] = M[x%04hX]  = x%hX; ", l->dr, (unsigned short) (cpu->pc+x), cpu->mem[(cpu->pc+x)], cpu->mem[cpu->mem[(cpu->pc+x)]]);
    cpu->reg[l->dr] = cpu->mem[cpu->mem[(cpu->pc+x)]];
    set_cc(cpu, cpu->mem[cpu->mem[(cpu->pc+x)]], instr);
    // FILL ME IN
}
               
               
        /*
         * Execute store indirect: Store source register to indirect
         * pc+offset memory location.
         *
         * Echo source, base, offset, base+offset, value at base+offset,
         * value stored.
         *
         * NOTE: sti does NOT modify condition codes
         *
         * FILL ME IN
         * ----------------------------------------------- DONE
         */

static void
instr_sti (cpu_t *cpu, instr_t *instr)
{
    st_sti_t *s   = (st_sti_t*)instr;
    
    // FILL ME IN
    int x = sext(s->pcoffset9, 9);
    
    printf("; M[M[x%04hX]]= M[x%04hX] <- x%hX\n", (unsigned short) (cpu->pc+x), cpu->mem[(cpu->pc+x)], cpu->reg[s->sr]);
    cpu->mem[cpu->mem[(cpu->pc+x)]] = cpu->reg[s->sr];
    // FILL ME IN
}
               
               
        /*
         * Execute jump instruction: Set PC to base register.
         *
         * Echo base register and value.
         *
         * NOTE: jmp does NOT modify condition codes
         *
         * FILL ME IN
         * ----------------------------------------------- DONE
         */
static void
instr_jmp (cpu_t *cpu, instr_t *instr)
{
    jmp_ret_t *j = (jmp_ret_t*)instr;
    
    // FILL ME IN
    cpu->reg[7] = cpu->pc;
    printf("; PC <- x%hX\n", (unsigned short) cpu->reg[j->baser]);
    cpu->pc = cpu->reg[j->baser];
    // FILL ME IN
}
               
               
        /*
         * Reserved opcode causes prints message but continues execution.
         * ----------------------------------------------- DONE
         */
static void
instr_err (cpu_t *cpu, instr_t *instr)
{

    printf(";Reserved opcode; ignored.\n");

}
               
               
        /*
         * Execute load effective address: Load destination register
         * with base-offset memory location.  Set condition code.
         *
         * Echo destination, base, offset, base+offset, new condition code.
         *
         * NOTE: lea DOES modify condition codes
         *
         * FILL ME IN
         * ----------------------------------------------- DONE
         */
static void
instr_lea (cpu_t *cpu, instr_t *instr)
{
    ld_ldi_lea_t *l = (ld_ldi_lea_t*)instr;
    int x = sext(l->pcoffset9, 9);
    // FILL ME IN

    printf("; R%d <- x%hX; ", l->dr, (unsigned short) (cpu->pc+x));
    cpu->reg[l->dr] = (cpu->pc+x);
    set_cc(cpu, (cpu->pc+x), instr);
    // FILL ME IN
}
               
               
        /*
         * Read and return a character from standard input.  User must
         * enter return after the char.  Just pressing return causes '\n'
         * to be returned.  Any extra characters after the first are ignored.
         * ----------------------------------------------- DONE
         */
static char
read_char (void)
{
    char buffer[3] = "";
    fgets(buffer, sizeof(buffer), stdin);
    return buffer[0];

}
               

static int
char_part (word_t w)
{
    return sign_ext(w, 7, 0);
}
               
               
        /*
         * Execute trap instruction according to trap vector.  (Set R7
         * to return location first.)
         *
         * TRAP x20 (GETC) Read char from stdin to R0[7:0].
         * TRAP x23 (IN)   Like GETC but prompt first and echo the char.
         * TRAP x21 (OUT)  Print char (whose ASCII repr. is) in R0[7:0]
         * TRAP x22 (PUTS) Print string starting at M[R0], stop at \0.
         * TRAP x25 (HALT) Halt execution (set CPU running flag to false).
         * Bad trap vectors cause an error message and halt.
         *
         * Echo vector in all cases.  Echo char read in for GETC, IN,
         * echo char printed for OUT.
         *
         * NOTE: SOME of these trap variants modify condition codes
         *
         * FILL ME IN
         *
         */
static void
instr_trap (cpu_t *cpu, instr_t *instr)
{
    trap_t *t = (trap_t*)instr;
    cpu->reg[7] = (word_t)(cpu->pc); // save ret addr
    
    char ch;	                // character read by GETC or IN
    word_t left_mask  = 0xff00;   // To select left byte of a Word
    word_t right_mask = 0x00ff;   // To select right byte of a Word
            
    printf("; ");
            
    switch (t->trapvect8) {
        case 0x20:
        case 0x23: // GETC, IN: Set R0 <- read-in char
                   // FILL ME IN: Condition code should be set by return address! (use set_cc)
            if (t->trapvect8 == 0x20) {
                set_cc(cpu, cpu->reg[7], instr);
                printf("; GETC: ");
            }
            else {
                set_cc(cpu, cpu->reg[7], instr);
                printf("; IN: Input a character>");
            }
                    
            ch = read_char();
                    
            // Only set the right half of R0 with the char
            
            cpu->reg[0] = (left_mask & cpu->reg[0]) | (right_mask & ch);
            printf("Read %c = %d\n", (char) ch, ch);
            
            break;
            
        case 0x21: // OUT: Print char in (right byte of) R0
            // FILL ME IN: set ch appropriately (using char_part() utility
            // function). Also set the condition code appropriately.
            ch = char_part(right_mask & cpu->reg[0]);
            set_cc(cpu, cpu->reg[7], instr);
            printf("; OUT: %d = %c\n", ch, (char) ch);
            break;
        
        case 0x22: // PUTS: Print string at R0
                    
            set_cc(cpu, cpu->reg[7], instr); // set CC by return addr
            printf("; PUTS: ");
            address_t loc = cpu->reg[0];
            ch = (char) char_part(cpu->mem[loc]);
                    
            // FILL ME IN: print out the entire string!
            int i = loc;
            while(cpu->mem[i] != 0x0000)
            {
                printf("%c",cpu->mem[i]);
                i++;
            }
            printf("\n");
            break;
            
        case 0x25:  // HALT execution
                    // FILL ME IN: should set running flag properly
                    // and set condition code to positive for halt
            set_cc(cpu, 1, instr);
            printf("; Halting\n");
            cpu->running = 0;
            break;
            
        default:
            printf("; Bad trap vector (halting)\n");
            cpu->running = 0;
            return;
    }
}
               
               
        /* -------------------- INSTRUCTION CYCLE ROUTINES -------------------- */
               
               
        /*
         * Execute one instruction cycle
         */
static void
one_instruction_cycle (cpu_t *cpu)
{
    // FILL ME IN: test if the cpu is running. If it isn't, print
    // "Halted\n" and return.
    
    if (cpu->running == 0){
        printf("Halted\n");
        return;
    }
            
    address_t instr_loc = cpu->pc;  // Instruction's addr (pc before increment)
    
    // FILL ME IN: load an instruction into IR and increment PC
    cpu->ir = cpu->mem[cpu->pc];
    cpu->pc+=1;
            
    // DECODE
    instr_t instr;
    decode_instr(cpu->ir, &instr); //DONE
    // Echo instruction
    print_addr_val_hex_instr(instr_loc, &instr); //DONE
    
    switch (instr.opcode) {
        case  0:  // this is branch
            instr_br(cpu, &instr);
            break;
        case  1:
            instr_add(cpu, &instr);
            break;
        case  2:
            instr_ld(cpu, &instr);
            break;
        case  3:
            instr_st(cpu, &instr);
            break;
        case  4:
            instr_jsr(cpu, &instr);
            break;
        case  5:
            instr_and(cpu, &instr);
            break;
        case  6:
            instr_ldr(cpu, &instr);
            break;
        case  7:
            instr_str(cpu, &instr);
            break;
        case  8:
            instr_rti(cpu, &instr);
            break;
        case  9:
            instr_not(cpu, &instr);
            break;
        case 10:
            instr_ldi(cpu, &instr);
            break;
        case 11:
            instr_sti(cpu, &instr);
            break;
        case 12:
            instr_jmp(cpu, &instr);
            break;
        case 13:
            instr_flt(cpu, &instr);
            break;
        case 14:
            instr_lea(cpu, &instr);
            break;
        case 15:
            instr_trap(cpu, &instr);
            break;
        default:
        {
            printf("Bad opcode: %d; quitting\n", instr.opcode);
            cpu->running = 0;
            break;
        }
    }
}
               
        /*
         * Execute a number of instruction cycles.  Exceptions: If the
         * number of cycles is <= 0, complain and return; if the CPU is
         * not running, say so and return; if the number of cycles is
         * too big, substitute a saner limit.
         *
         * If, as we execute the many cycles, the CPU stops running,
         * then return.
         */
               static void
               many_instruction_cycles (int nbr_cycles, cpu_t *cpu)
        {
            if (nbr_cycles < 1) {
                printf("Number of instruction cycles to do should be > 0\n");
                return;
            } else if (!cpu -> running) {
                printf("Halted\n");
                return;
            }
            
            if (nbr_cycles > MULTI_INSTR_LIMIT) {
                nbr_cycles = MULTI_INSTR_LIMIT;
            }
            
            int cycle;
            for (cycle = 0; cpu -> running && cycle < nbr_cycles; cycle++) {
                one_instruction_cycle(cpu);
            }
        }
               
               
        /*
         * Execute a nonnumeric command; complain if it's not ? h d j m q r
         * Return true for the q command, false otherwise
         */
static int
exec_cmd (char cmd_char, char *command, cpu_t *cpu)
{
    
    switch (cmd_char) {
        case '?':
        case 'h':
        case 'H':
        {
            help_msg(); // DONE
            break;
        }
        case 'd':
        case 'D':
        {
            dump_cpu(cpu); // DONE
            dump_memory(cpu, 0, 0xffff); // DONE
            break;
        }
        case 'q':
        case 'Q':
        {
            printf("Quitting\n");
            return 1;
        }
        case '\n':
        {
            one_instruction_cycle(cpu);
            break;
        }
        default:
        {
            printf("There is no %c command.\n", cmd_char);
            break;
        }
    }
    return 0;
}
               
               
        /*
         * Read a simulator command from the keyboard ("h", "?", "d", number,
         * or empty line) and execute it.  Return true if we hit end-of-input
         * or execute_command told us to quit.  Otherwise return false.
         *
         * FILL ME IN: finish this
         */
static int
read_exec_cmd (cpu_t *cpu)
{
    int nbr_cycles;						// nbr of instr cycles to do
    char cmd_char;						// command if not number
    int done = 0;						// Should simulator stop?
    
#define CMD_BUFFER_LEN 80
    char cmd_buffer[CMD_BUFFER_LEN];	// Text of current line
    char *read_success;         		// NULL if reading in a line fails.
    int value_read = 0;
    
            /*
             * FILL ME IN: Read in a command line using fgets; if there wasn't one, we're done
             * else use sscanf on the command line buffer to read an integer
             * number of cycles. If the sscanf succeeds, execute that many
             * instruction cycles (by calling many_instruction_cycles appropriately).
             * Else use sscanf on the command line buffer
             * to read in a character cmd_char ('q', 'h', '?', 'd', or '\n')
             * and call exec_cmd on cmd_char
             *
             * HINT: you can probably recycle your lab6 solution here...
             */
    read_success = fgets(cmd_buffer, CMD_BUFFER_LEN, stdin);
    if (read_success == NULL){
        done = 1;
    }
    int loc=0;
    
    value_read = cmd_buffer[loc];
    int x = sscanf(cmd_buffer, "%d", &value_read);
    
    if (x > 0){
        nbr_cycles = value_read;
        many_instruction_cycles(nbr_cycles, cpu);
    }
    else{
        cmd_char = *read_success;
        done = exec_cmd(cmd_char, cmd_buffer, cpu);
    }

//    int success = sscanf(cmd_buffer, "%d", &nbr_cycles);
//        if (success>0) {
//            many_instruction_cycles(nbr_cycles, cpu);  //DONE
//            return done;
//        }
//        else {
//            sscanf(cmd_buffer, "%c", &cmd_char);
//            done = exec_cmd(cmd_char, &cmd_char, cpu) == 1;
//        }
//    }

    return done;
}

        /*
         *  XXX
         * "static" means this function can only be referenced in this file, not
         * another. This is not quite the same as the static keyword in Java, which you
         * may be used to. "Inline" is a performance optimization.  Once the compiler
         * turns this into machine code, it will look as this function is not a function
         * at all. The code will be "inlined" into whatever function that calls it. This
         * increases code size, but reduces overhead from function calls. You typically
         * use this for small helper functions that perform quick tasks.
         *
         */
static inline void
clear_mem (cpu_t *cpu)
{
    int i;
            
    for (i = 0; i < MEMLEN; i++) {
        cpu->mem[i] = 0;
    }
}

static inline void clear_reg (cpu_t *cpu){
    int j;
    for (j = 0; j < NREG; j++) {
        cpu->reg[j] = 0;
    }
}

static inline void clear_ir(cpu_t *cpu)
{
    cpu->ir = 0;
}

static inline void clear_pc(cpu_t *cpu)
{
    cpu->pc = 0;
}
               
        /*
         * Initialize the CPU (pc, ir, condition codes, running flag, 
         * GPRs).
         *
         * FILL ME IN: you should clear everything in the control unit to zero. 
         * The initial condition code should have zero flag set. Also clear all the
         * GPRs.
         */

void
init_cpu (cpu_t *cpu)
{
    // FILL ME IN
    // zero memory initially just to be safe
    clear_mem(cpu);
    
    // X FILL ME IN: use your code from lab 5 to zero the register file
    // and reset the control unit
    clear_reg(cpu);
    cpu->cc = 2;
    clear_ir(cpu);
    clear_pc(cpu);

    
    cpu->running = 1;
    
}
               
               
/*
 * Get the data file to initialize memory with.  If it was specified on the
 * command line as argv[1], use that file otherwise use default.hex.  If file
 * opening fails, complain and terminate progra:m execution with an error.  See
 * linux command man 3 exit for details.
 */
FILE *get_datafile (int argc, char *argv[])
{
    char *default_datafile_name = "default.hex";
    char *datafile_name;
    FILE *datafile = NULL;
    
    datafile_name = argv[1];
    
    printf("Loading %s\n", datafile_name);
    
    // FILL ME IN: open the file. If it couldn't be opened,
    // print "ERROR: could not open file.\n" and exit with
    // EXIT_FAILURE
    if (datafile_name != NULL){
        datafile = fopen(datafile_name, "r");
        if (datafile == NULL){
            printf("ERROR: could not open file.\n");
            exit(EXIT_FAILURE);
        }
        else{
            return datafile;
        }
    }
    else{
        datafile = fopen(default_datafile_name, "r");
        if (datafile == NULL){
            printf("ERROR: could not open file.\n");
            exit(EXIT_FAILURE);
        }
        else{
            return datafile;
        }
    }
}

//    The working code
    
//    datafile = fopen(datafile_name, "r");
//    
//    if(datafile == NULL){
//        printf("ERROR: could not open file.\n");
//        exit(0);
//    }
//    else{
//        datafile = fopen(default_datafile_name, "r");
//    }
//    if (datafile == NULL) {
//        printf("ERROR: could not open file.\n");
//        exit(3);
//    }
//     return datafile;
//}
    
               
/*
 * Read and dump initial values for memory
 *
 * FILL ME IN
 *
 */

void
init_memory (int argc, char *argv[], cpu_t *cpu)
{
    FILE *datafile = get_datafile(argc, argv);
    unsigned int value_read = 0;
            
    // FILL ME IN First set all of memory to zero.  Note: because addresses
    // are unsigned, we can't test for a address > MEMLEN; we
    // have to check for cycling back around to zero.
    //clear_mem(cpu); // don't forget the cycle BUG
    // int i=0;
    //address_t i = 0;
    memset(cpu->mem, 0, MEMLEN*sizeof(word_t));
    
            
    // FILL ME IN: use read_hex_number() to read in the origin (this is
    // the first hex value in the file. If you couldn't read it, complain
    // with "ERROR: Couldn't read origin; quitting\n" and exit with EXIT FAILURE
    int test = read_hex_number(datafile, &value_read);
    if( test == 1){
        cpu->pc = value_read;
        printf("Origin = x%04hX\n", (word_t) value_read);
    } else{
        printf("ERROR: Couldn't read origin; quitting\n");
        exit(EXIT_FAILURE);
    }
    
    // FILL ME IN: set PC to the origin
    
    // FILL ME IN: use read_hex_number() repeatedly to
    // read in values from the file to fill in memory starting
    // at whatever address the origin was
    int k=cpu->pc;
    int n = read_hex_number(datafile, &value_read);
    
    while(n != 0){
        cpu->mem[k]=value_read;
        if (k == 0xFFFF){
            cpu->mem[k] = value_read;
            k=0x0000;
            k=0;
        }
        else{
            k++;
        }
        n = read_hex_number(datafile, &value_read);
    }
    fclose(datafile);
    printf("\n");
}
               
               
        /*
         * dump_cpu (cpu_t *cpu): Print out the control unit
         * and general-purpose registers (GPRs)
         *
         */
void
dump_cpu (cpu_t *cpu)
{
    printf("CPU STATE:\n");
    printf("PC = x%04hX   ", cpu->pc);
    printf("IR = x%04hX   ", cpu->ir);
    printf("CC = %c   ", cc2char(cpu));
    printf("RUNNING: %d\n", cpu->running);
    dump_gprs(cpu);
    printf("\n");
}
               
               
/*
 * dump_memory(cpu_t *cpu, from, to): Print memory values
 * with addresses from, from+1, ..., to (possibly wrapping
 * around xFFFF to x0000).
 */
void
dump_memory (cpu_t *cpu, address_t from, address_t to)
{   // FFFE from   and FFFD too
    if (to == (address_t) (from - 1)) {
        printf("MEMORY (from x%04hX):\n", from);
    }
    else {
        printf("MEMORY (locations x%04hX to x%04hX):\n", from, to);
    }
            
    address_t addr = 0;
    for (addr = from; addr != to; addr++) {
        instr_t instr;
        decode_instr(cpu->mem[addr], &instr);
        print_addr_val_hex_dec_instr(addr, &instr);
    }
    
    instr_t instr;
    decode_instr(cpu->mem[to], &instr);
    print_addr_val_hex_dec_instr(addr, &instr);
    printf("\n");
}
               
               
/*
 * dump_registers(cpu_t *cpu): Print register values in two rows of
 * five.
 */
void
dump_gprs (cpu_t *cpu)
{
    int regn;
    word_t w;
    for (regn = 0 ; regn < NREG ; regn++) {
        w = cpu->reg[regn];
        printf("R%d x%04hX %- 6d%s", regn, w, w, (regn % 4 == 3 ? "\n" : "   "));
    }
}
               
               
        /*
         * Main program: Initialize the cpu, read in a program,
         * and execute it
         *
         * FILL ME IN: finish this function
         */
int
main (int argc, char *argv[])
{
    cpu_t cpu_value, *cpu = &cpu_value;
    
    printf("CS 350 Final Project: LC-3 Simulator ");
    
    if (argc != 2) {
        usage(argv);
    }
            
    init_cpu(cpu); // definitely correct
    init_memory(argc, argv, cpu); // possibly not correct
    
    dump_cpu(cpu); //good
    dump_memory(cpu, cpu->pc, cpu->pc-1); // dump from .ORIG  // good
    
    char *prompt = "$> ";
    printf("\nBeginning execution; type h for help\n%s", prompt);
            
    // FILL ME IN: repeatedly call read_exec_cmd until it indicates that
    // the CPU should stop
    int run = read_exec_cmd(cpu);
    while(!run){
        printf("%s", prompt);
        run = read_exec_cmd(cpu);
    }
    return 0;
}
