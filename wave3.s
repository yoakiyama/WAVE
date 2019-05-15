;;; A WIND emulator for the WARM ISA
;;; (c) 2019, Yo Akiyama & Jack Murphy
;;; -*- mode: asm; compile-command: "wia wave2.s" -*-
	.requ	ir,r2		;instruction register
	.requ	dest,r3		;destination register
	.requ	fsrc,r4		;first source (left)
	.requ	ssrc,r5		;second source (right)
	.requ	shft,r6		;shift mode and/or shift op
	.requ	cond,r7		;conditional instructions
	.requ	op,r8		;opcode
	.requ	warmccr,r9	;store WARM ccr here
	
	lea	warm,r0
	trap	$SysOverlay
loop:	and	$0xFFFFFF,wpc
	mov	wpc,r1		; TOP OF INSTRUCTION FETCH LOOP
	mov	warm(r1),cond
	shr	$29,cond	;store conditionals
	mov	condTab(cond),rip
	
opStor:	mov	warm(r1),op		
	shr	$23,op
	and	$0x3F,op	;opcode stored (includes s bit [28th bit])
	mov	optype(op),rip	;jump accordingly to arith,mem,or branch

condTab:	.data	opStor,nv,eq,ne,lt,le,ge,gt

nv:	add	$1,wpc
	and	$0xFFFFFF,wpc	;BEGIN LOOP AGAIN
	mov	wpc,r1		; TOP OF INSTRUCTION FETCH LOOP
	mov	warm(r1),cond
	shr	$29,cond	;store conditionals
	mov	condTab(cond),rip

eq:	mov	warmccr,ccr	 
	je	opStor		
	add	$1,wpc		;otherwise skip
	and	$0xFFFFFF,wpc	;BEGIN LOOP AGAIN
	mov	wpc,r1		; TOP OF INSTRUCTION FETCH LOOP
	mov	warm(r1),cond
	shr	$29,cond	;store conditionals
	mov	condTab(cond),rip

ne:	mov	warmccr,ccr	 
	jne	opStor		
	add	$1,wpc		;otherwise skip
	and	$0xFFFFFF,wpc	;BEGIN LOOP AGAIN
	mov	wpc,r1		; TOP OF INSTRUCTION FETCH LOOP
	mov	warm(r1),cond
	shr	$29,cond	;store conditionals
	mov	condTab(cond),rip

lt:	mov	warmccr,ccr	 
	jl	opStor		
	add	$1,wpc		;otherwise skip
	and	$0xFFFFFF,wpc	;BEGIN LOOP AGAIN
	mov	wpc,r1		; TOP OF INSTRUCTION FETCH LOOP
	mov	warm(r1),cond
	shr	$29,cond	;store conditionals
	mov	condTab(cond),rip

le:	mov	warmccr,ccr	 
	jle	opStor		
	add	$1,wpc		;otherwise skip
	and	$0xFFFFFF,wpc	;BEGIN LOOP AGAIN
	mov	wpc,r1		; TOP OF INSTRUCTION FETCH LOOP
	mov	warm(r1),cond
	shr	$29,cond	;store conditionals
	mov	condTab(cond),rip

ge:	mov	warmccr,ccr	 
	jge	opStor		
	add	$1,wpc		;otherwise skip
	and	$0xFFFFFF,wpc	;BEGIN LOOP AGAIN
	mov	wpc,r1		; TOP OF INSTRUCTION FETCH LOOP
	mov	warm(r1),cond
	shr	$29,cond	;store conditionals
	mov	condTab(cond),rip

gt:	mov	warmccr,ccr	 
	jg	opStor		
	add	$1,wpc		;otherwise skip
	and	$0xFFFFFF,wpc	;BEGIN LOOP AGAIN
	mov	wpc,r1		; TOP OF INSTRUCTION FETCH LOOP
	mov	warm(r1),cond
	shr	$29,cond	;store conditionals
	mov	condTab(cond),rip


arith:	mov	warm(r1),dest		;decode arithmetic ops
	shr	$19,dest
	and	$0xF,dest	;store dest reg
	mov	warm(r1),fsrc
	shr	$15,fsrc
	and	$0xF,fsrc	;store 1st source reg
	mov	wregs(fsrc),fsrc ;store contents of source reg
	mov	warm(r1),shft
	shr	$10,shft
	and	$0x1F,shft	;store shift mode/shop
	mov	ashift(shft),rip ;jump to correct shift type
	
immed:	mov	warm(r1),ssrc
	and	$0x1FF,ssrc	;store value in ssrc
	mov	warm(r1),ir
	shr	$9,ir
	and	$0x1F,ir	
	add	$1,wpc
	shl	ir,ssrc		;condition codes now set for ssrc
	mov	opjmp(op),rip	;jump to correct binary operation

LSLimm:	mov	warm(r1),ssrc
	shr	$6,ssrc
	and	$0xF,ssrc	;store second source register
	mov	wregs(ssrc),ssrc ;store contents of 2nd src reg
	mov	warm(r1),ir
	and	$0x3F,ir	;store shift
	add	$1,wpc
	shl	ir,ssrc		;condition codes now set for ssrc
	mov	opjmp(op),rip

LSRimm:	mov	warm(r1),ssrc
	shr	$6,ssrc
	and	$0xF,ssrc	;store second source register
	mov	wregs(ssrc),ssrc ;store contents of 2nd src reg
	mov	warm(r1),ir
	and	$0x3F,ir	;store shift
	add	$1,wpc
	shr	ir,ssrc		;condition codes now set for wpc
	mov	opjmp(op),rip
	
ASRimm:	mov	warm(r1),ssrc
	shr	$6,ssrc
	and	$0xF,ssrc	;store second source register
	mov	wregs(ssrc),ssrc ;store contents of 2nd src reg
	mov	warm(r1),ir
	and	$0x3F,ir	;store shift
	add	$1,wpc
	sar	ir,ssrc		;condition codes now set for wpc
	mov	opjmp(op),rip

RORimm:	mov	warm(r1),ssrc
	shr	$6,ssrc
	and	$0xF,ssrc	;store second source register
	mov	wregs(ssrc),ssrc ;store contents of 2nd src reg
	mov	warm(r1),ir
	and	$0x3F,ir	;store shift
	;; Do we need to store the lowest 5 or 6 bits?? (Max shift supposed to
	;; be 31, not 63)

	mov	$1,r10
	shl	ir,r10
	sub	$1,r10		;r10 contains 'zdec' 1's
	and	ssrc,r10	;now r10 contains the lower 'zdec' bits of ssrc
	shr	ir,ssrc		;shr fills high bits with zeroes
	mov	$32,r11
	sub	ir,r11
	shl	r11,r10		;circle the low bits to the highest bits
	add	$1,wpc
	add	r10,ssrc	;synthesize the two parts into ssrc. Condition codes set for ssrc
	mov	opjmp(op),rip

LSLreg:	mov	warm(r1),ssrc
	shr	$6,ssrc
	and	$0xF,ssrc	;store second source register
	mov	wregs(ssrc),ssrc ;store contents of 2nd src reg
	mov	warm(r1),ir
	and	$0xF,ir	;store shift reg
	mov	wregs(ir),ir ;store shift value 
	shl	ir,ssrc
	add	$1,wpc
	mov	opjmp(op),rip

LSRreg:	mov	warm(r1),ssrc
	shr	$6,ssrc
	and	$0xF,ssrc	;store second source register
	mov	wregs(ssrc),ssrc ;store contents of 2nd src reg
	mov	warm(r1),ir
	and	$0xF,ir	;store shift reg
	mov	wregs(ir),ir ;store shift value
	shr	ir,ssrc
	add	$1,wpc
	mov	opjmp(op),rip

ASRreg:	mov	warm(r1),ssrc
	shr	$6,ssrc
	and	$0xF,ssrc	;store second source register
	mov	wregs(ssrc),ssrc ;store contents of 2nd src reg
	mov	warm(r1),ir
	and	$0xF,ir	;store shift reg
	mov	wregs(ir),ir ;store shift value
	sar	ir,ssrc
	add	$1,wpc
	mov	opjmp(op),rip

RORreg:	mov	warm(r1),ssrc
	shr	$6,ssrc
	and	$0xF,ssrc	;store second source register
	mov	wregs(ssrc),ssrc ;store contents of 2nd src reg
	mov	warm(r1),ir
	and	$0xF,ir	;store shift reg
	mov	wregs(ir),ir ;store shift value

	mov	$1,r10
	shl	ir,r10
	sub	$1,r10		;r10 contains 'zdec' 1's
	and	ssrc,r10	;now r10 contains the lower 'zdec' bits of ssrc
	shr	ir,ssrc		;shr fills high bits with zeroes
	mov	$32,r11
	sub	ir,r11
	shl	r11,r10		;circle the low bits to the highest bits
	add	r10,ssrc	;synthesize the two parts into ssrc
	add	$1,wpc
	mov	opjmp(op),rip

mem:	mov	warm(r1),dest
	shr	$19,dest
	and	$0xF,dest
	mov	warm(r1),fsrc
	shr	$15,fsrc
	and	$0xF,fsrc	;Stores the register number
	mov	wregs(fsrc),r11
	and	$0xFFFFFF,r11
	mov	warm(r1),shft
	shr	$10,shft
	and	$0x1F,shft
	mov	mshift(shft),rip

memimm:	mov	warm(r1),ssrc
	add	$1,wpc
	shl	$18,ssrc
	sar	$18,ssrc	;*****must sign extend to set ccr!!!!!!!!
	mov	opjmp(op),rip
	

cmptst:	mov	warm(r1),fsrc		;****Same as arith but no dest reg. needed
	shr	$15,fsrc
	and	$0xF,fsrc	;store 1st source reg
	mov	wregs(fsrc),fsrc ;store contents of source reg
	mov	warm(r1),shft
	shr	$10,shft
	and	$0x1F,shft	;store shift mode/shop
	mov	ashift(shft),rip ;jump to correct shift type

move:	mov	warm(r1),dest		;decode arithmetic ops
	shr	$19,dest
	and	$0xF,dest	;store dest reg
	mov	warm(r1),shft
	shr	$10,shft
	and	$0x1F,shft	;store shift mode/shop
	mov	ashift(shft),rip ;jump to correct shift type

moveP:	mov	warm(r1),dest
	shr	$19,dest
	and	$0xF,dest
	mov	warm(r1),r0
	trap	$SysPLA
	mov	plashiftmov(r0),rip

plashiftmov:	.data	movimmexpr,movimm,LSLimm,LSRimm,ASRimm,RORimm,LSLreg,LSRreg,ASRreg,RORreg

movimmexpr:
	mov	warm(r1),ssrc
	and	$0x1FF,ssrc
	add	$1,wpc
	mov	ssrc,wregs(dest)
	and	$0xFFFFFF,wpc	;BEGIN LOOP AGAIN
	mov	wpc,r1		; TOP OF INSTRUCTION FETCH LOOP
	mov	warm(r1),cond
	shr	$29,cond	;store conditionals
	mov	condTab(cond),rip

movimm:
	mov	warm(r1),ssrc
	and	$0x1FF,ssrc
	mov	warm(r1),ir
	shr	$9,ir
	and	$0x1F,ir
	add	$1,wpc
	shl	ir,ssrc
	mov	ssrc,wregs(dest)
	and	$0xFFFFFF,wpc	;BEGIN LOOP AGAIN
	mov	wpc,r1		; TOP OF INSTRUCTION FETCH LOOP
	mov	warm(r1),cond
	shr	$29,cond	;store conditionals
	mov	condTab(cond),rip
	
halt:	trap 	$SysHalt

mshift:	.data	memimm,memimm,memimm,memimm,memimm,memimm,memimm,memimm
	.data	memimm,memimm,memimm,memimm,memimm,memimm,memimm,memimm
	.data	LSLimm,LSRimm,ASRimm,RORimm,halt,halt,halt,halt
	.data	halt,halt,halt,halt,halt,halt,halt,halt

ashift:	.data	immed,immed,immed,immed,immed,immed,immed,immed
	.data	immed,immed,immed,immed,immed,immed,immed,immed
	.data	LSLimm,LSRimm,ASRimm,RORimm,LSLreg,LSRreg,ASRreg,RORreg
	.data	halt,halt,halt,halt,halt,halt,halt,halt	;reg. sh. product dealt with in mla

optype:	.data	arith,arith,arith,cmptst,arith,arith,arith,cmptst ;cmptst
	.data	arith,mla,arith,moveP,move,swi,move,move ;mla,mov,mvn special
	.data	mem,mem,mem,mem,mem,halt,halt,halt
	.data	b,b,bl,bl,halt,halt,halt,halt
	.data	arith,arith,arith,cmptst,arith,arith,arith,cmptst ;cmptst
	.data	arith,mlas,arith,move,move,swis,halt,halt ;mla,mov,mvn special
	.data	mem,mem,mem,mem,mem,halt,halt,halt
	.data	b,b,bl,bl,halt,halt,halt,halt

opjmp:	.data	add,addc,sub,halt,eor,orr,and,halt 
	.data	mul,halt,div,mov,mvn,halt,ldm,stm
	.data	ldr,str,ldu,stu,adr,halt,halt,halt
	.data	halt,halt,halt,halt,halt,halt,halt,halt
	.data	adds,addcs,subs,cmp,eors,orrs,ands,tst 
	.data	muls,halt,divs,movs,mvns,halt,halt,halt ;no ldms or stms
	.data	ldrs,strs,ldus,stus,halt,halt,halt,halt
	.data	halt,halt,halt,halt,halt,halt,halt,halt

add:	lea	0(ssrc,fsrc),wregs(dest) ;add with lea
	and	$0xFFFFFF,wpc		 ;Begin loop 
	mov	wpc,r1		; TOP OF INSTRUCTION FETCH LOOP
	mov	warm(r1),cond
	shr	$29,cond	;store conditionals
	mov	condTab(cond),rip

adds:	add	ssrc,fsrc
	mov	ccr,warmccr
	mov	fsrc,wregs(dest)
	and	$0xFFFFFF,wpc
	mov	wpc,r1		; TOP OF INSTRUCTION FETCH LOOP
	mov	warm(r1),cond
	shr	$29,cond	;store conditionals
	mov	condTab(cond),rip

addc:	add	ssrc,fsrc
	mov	warmccr,r10
	shr	$1,r10
	and	$1,r10		
	lea	0(r10,fsrc),wregs(dest)
	and	$0xFFFFFF,wpc	;BEGIN LOOP AGAIN
	mov	wpc,r1		; TOP OF INSTRUCTION FETCH LOOP
	mov	warm(r1),cond
	shr	$29,cond	;store conditionals
	mov	condTab(cond),rip
	
addcs:	mov	warmccr,r10
	shr	$1,r10
	and	$1,r10
	add	ssrc,fsrc
	mov	ccr,warmccr
	lea	0(r10,fsrc),wregs(dest)
	and	$0xFFFFFF,wpc	;BEGIN LOOP AGAIN
	mov	wpc,r1		; TOP OF INSTRUCTION FETCH LOOP
	mov	warm(r1),cond
	shr	$29,cond	;store conditionals
	mov	condTab(cond),rip

sub:	sub	ssrc,fsrc
	mov	fsrc,wregs(dest)
	and	$0xFFFFFF,wpc	;BEGIN LOOP AGAIN
	mov	wpc,r1		; TOP OF INSTRUCTION FETCH LOOP
	mov	warm(r1),cond
	shr	$29,cond	;store conditionals
	mov	condTab(cond),rip

subs:	sub	ssrc,fsrc
	mov	ccr,warmccr
	mov	fsrc,wregs(dest)
	and	$0xFFFFFF,wpc	;BEGIN LOOP AGAIN
	mov	wpc,r1		; TOP OF INSTRUCTION FETCH LOOP
	mov	warm(r1),cond
	shr	$29,cond	;store conditionals
	mov	condTab(cond),rip
	
cmp:	sub	ssrc,fsrc	
	mov	ccr,warmccr
	and	$0xFFFFFF,wpc	;BEGIN LOOP AGAIN
	mov	wpc,r1		; TOP OF INSTRUCTION FETCH LOOP
	mov	warm(r1),cond
	shr	$29,cond	;store conditionals
	mov	condTab(cond),rip

eor:	xor	ssrc,fsrc
	mov	fsrc,wregs(dest)
	and	$0xFFFFFF,wpc	;BEGIN LOOP AGAIN
	mov	wpc,r1		; TOP OF INSTRUCTION FETCH LOOP
	mov	warm(r1),cond
	shr	$29,cond	;store conditionals
	mov	condTab(cond),rip

eors:	xor	ssrc,fsrc
	mov	ccr,warmccr
	mov	fsrc,wregs(dest)
	and	$0xFFFFFF,wpc	;BEGIN LOOP AGAIN
	mov	wpc,r1		; TOP OF INSTRUCTION FETCH LOOP
	mov	warm(r1),cond
	shr	$29,cond	;store conditionals
	mov	condTab(cond),rip

orr:	or	ssrc,fsrc
	mov	fsrc,wregs(dest)
	and	$0xFFFFFF,wpc	;BEGIN LOOP AGAIN
	mov	wpc,r1		; TOP OF INSTRUCTION FETCH LOOP
	mov	warm(r1),cond
	shr	$29,cond	;store conditionals
	mov	condTab(cond),rip

orrs:	or	ssrc,fsrc
	mov	ccr,warmccr
	mov	fsrc,wregs(dest)
	and	$0xFFFFFF,wpc	;BEGIN LOOP AGAIN
	mov	wpc,r1		; TOP OF INSTRUCTION FETCH LOOP
	mov	warm(r1),cond
	shr	$29,cond	;store conditionals
	mov	condTab(cond),rip

and:	and	ssrc,fsrc
	mov	fsrc,wregs(dest)
	and	$0xFFFFFF,wpc	;BEGIN LOOP AGAIN
	mov	wpc,r1		; TOP OF INSTRUCTION FETCH LOOP
	mov	warm(r1),cond
	shr	$29,cond	;store conditionals
	mov	condTab(cond),rip

ands:	and	ssrc,fsrc
	mov	ccr,warmccr
	mov	fsrc,wregs(dest)
	and	$0xFFFFFF,wpc	;BEGIN LOOP AGAIN
	mov	wpc,r1		; TOP OF INSTRUCTION FETCH LOOP
	mov	warm(r1),cond
	shr	$29,cond	;store conditionals
	mov	condTab(cond),rip
	
tst:	test	ssrc,fsrc	
	mov	ccr,warmccr
	and	$0xFFFFFF,wpc	;BEGIN LOOP AGAIN
	mov	wpc,r1		; TOP OF INSTRUCTION FETCH LOOP
	mov	warm(r1),cond
	shr	$29,cond	;store conditionals
	mov	condTab(cond),rip

mul:	mul	ssrc,fsrc
	mov	fsrc,wregs(dest)
	and	$0xFFFFFF,wpc	;BEGIN LOOP AGAIN
	mov	wpc,r1		; TOP OF INSTRUCTION FETCH LOOP
	mov	warm(r1),cond
	shr	$29,cond	;store conditionals
	mov	condTab(cond),rip

muls:	mul	ssrc,fsrc
	mov	ccr,warmccr
	mov	fsrc,wregs(dest)
	and	$0xFFFFFF,wpc	;BEGIN LOOP AGAIN
	mov	wpc,r1		; TOP OF INSTRUCTION FETCH LOOP
	mov	warm(r1),cond
	shr	$29,cond	;store conditionals
	mov	condTab(cond),rip

	;; optimize ************
mla:	mov	warm(r1),dest		;special register product mode
	shr	$19,dest
	and	$0xF,dest	;store dest reg
	mov	warm(r1),fsrc
	shr	$15,fsrc
	and	$0xF,fsrc	;store 1st source reg
	mov	wregs(fsrc),fsrc ;store contents of source reg
	mov	warm(r1),ssrc
	shr	$6,ssrc
	and	$0xF,ssrc
	mov	wregs(ssrc),ssrc ;2nd source stored
	mov	warm(r1),ir
	and	$0xF,ir		 ;don't need instruction any more, store in ir
	mov	wregs(ir),ir

	mul	ir,ssrc
	lea	0(ssrc,fsrc),wregs(dest)
	add 	$1,wpc
	jmp	loop

mlas:	mov	warm(r1),dest		;special register product mode
	shr	$19,dest
	and	$0xF,dest	;store dest reg
	mov	warm(r1),fsrc
	shr	$15,fsrc
	and	$0xF,fsrc	;store 1st source reg
	mov	wregs(fsrc),fsrc ;store contents of source reg
	mov	warm(r1),ssrc
	shr	$6,ssrc
	and	$0xF,ssrc
	mov	wregs(ssrc),ssrc ;2nd source stored
	mov	warm(r1),ir
	and	$0xF,ir		 ;don't need instruction any more, store in ir
	mov	wregs(ir),ir

	mul	ir,ssrc
	add	ssrc,fsrc
	mov	ccr,warmccr
	mov	fsrc,wregs(dest)
	add 	$1,wpc
	jmp	loop
	
div:	div	ssrc,fsrc
	mov	fsrc,wregs(dest)
	and	$0xFFFFFF,wpc	;BEGIN LOOP AGAIN
	mov	wpc,r1		; TOP OF INSTRUCTION FETCH LOOP
	mov	warm(r1),cond
	shr	$29,cond	;store conditionals
	mov	condTab(cond),rip

divs:	div	ssrc,fsrc
	mov	ccr,warmccr
	mov	fsrc,wregs(dest)
	and	$0xFFFFFF,wpc	;BEGIN LOOP AGAIN
	mov	wpc,r1		; TOP OF INSTRUCTION FETCH LOOP
	mov	warm(r1),cond
	shr	$29,cond	;store conditionals
	mov	condTab(cond),rip
	
mov:	mov	ssrc,wregs(dest)
	and	$0xFFFFFF,wpc	;BEGIN LOOP AGAIN
	mov	wpc,r1		; TOP OF INSTRUCTION FETCH LOOP
	mov	warm(r1),cond
	shr	$29,cond	;store conditionals
	mov	condTab(cond),rip

movs:	mov	ssrc,wregs(dest)
	add	$0,ssrc		;update wind ccr by calling add
	mov	ccr,warmccr	;does this clear C and V condition codes???
	and	$0xFFFFFF,wpc	;BEGIN LOOP AGAIN
	mov	wpc,r1		; TOP OF INSTRUCTION FETCH LOOP
	mov	warm(r1),cond
	shr	$29,cond	;store conditionals
	mov	condTab(cond),rip
	
mvn:	xor	$0xFFFFFFFF,ssrc ;complement ssrc
	mov	ssrc,wregs(dest)
	and	$0xFFFFFF,wpc	;BEGIN LOOP AGAIN
	mov	wpc,r1		; TOP OF INSTRUCTION FETCH LOOP
	mov	warm(r1),cond
	shr	$29,cond	;store conditionals
	mov	condTab(cond),rip

mvns:	xor	$0xFFFFFFFF,ssrc
	mov	ccr,warmccr	
	mov	ssrc,wregs(dest)
	and	$0xFFFFFF,wpc	;BEGIN LOOP AGAIN
	mov	wpc,r1		; TOP OF INSTRUCTION FETCH LOOP
	mov	warm(r1),cond
	shr	$29,cond	;store conditionals
	mov	condTab(cond),rip

swi:	mov	warm(r1),ssrc
	and	$0x1FF,ssrc	;store value in ssrc
	mov	warm(r1),ir
	shr	$9,ir
	and	$0x1F,ir	;store exponent in ir
	shl	ir,ssrc		;swi number now in ssrc
	mov	wr0,r0		;move potential output
	trap	ssrc
	mov 	r0,wr0 
	add	$1,wpc		;increment pc
	and	$0xFFFFFF,wpc	;BEGIN LOOP AGAIN
	mov	wpc,r1		; TOP OF INSTRUCTION FETCH LOOP
	mov	warm(r1),cond
	shr	$29,cond	;store conditionals
	mov	condTab(cond),rip

swis:	mov	warm(r1),ssrc
	and	$0x1ff,ssrc
	mov	warm(r1),ir
	shr	$9,ir
	and	$0x1F,ir
	shl	ir,ssrc
	mov	wr0,r0
	trap	ssrc
	mov	r0,wr0
	add	$0,r0		;update condition codes by calling add
	mov	ccr,warmccr
	add	$1,wpc		
	and	$0xFFFFFF,wpc	;BEGINE LOOP AGAIN
	mov	wpc,r1		; TOP OF INSTRUCTION FETCH LOOP
	mov	warm(r1),cond
	shr	$29,cond	;store conditionals
	mov	condTab(cond),rip

ldm:	mov	wregs(dest),r13
	and	$0xFFFFFF,r13

ldm0:	test	$1,ssrc		;test bit zero
	je	ldm1
	mov	warm(r13),wr0
	add	$1,r13
ldm1:	test	$0x2,ssrc		;test bit zero
	je	ldm2
	mov	warm(r13),wr1
	add	$1,r13
ldm2:	test	$0x4,ssrc		;test bit zero
	je	ldm3
	mov	warm(r13),wr2
	add	$1,r13
ldm3:	test	$0x8,ssrc		;test bit zero
	je	ldm4
	mov	warm(r13),wr3
	add	$1,r13
ldm4:	test	$0x10,ssrc		;test bit zero
	je	ldm5
	mov	warm(r13),wr4
	add	$1,r13
ldm5:	test	$0x20,ssrc		;test bit zero
	je	ldm6
	mov	warm(r13),wr5
	add	$1,r13
ldm6:	test	$0x40,ssrc		;test bit zero
	je	ldm7
	mov	warm(r13),wr6
	add	$1,r13
ldm7:	test	$0x80,ssrc		;test bit zero
	je	ldm8
	mov	warm(r13),wr7
	add	$1,r13
ldm8:	test	$0x100,ssrc		;test bit zero
	je	ldm9
	mov	warm(r13),wr8
	add	$1,r13
ldm9:	test	$0x200,ssrc		;test bit zero
	je	ldm10
	mov	warm(r13),wr9
	add	$1,r13
ldm10:	test	$0x400,ssrc		;test bit zero
	je	ldm11
	mov	warm(r13),wr10
	add	$1,r13
ldm11:	test	$0x800,ssrc		;test bit zero
	je	ldm12
	mov	warm(r13),wr11
	add	$1,r13
ldm12:	test	$0x1000,ssrc		;test bit zero
	je	ldm13
	mov	warm(r13),wr12
	add	$1,r13
ldm13:	test	$0x2000,ssrc		;test bit zero
	je	ldm14
	mov	warm(r13),wr13
	add	$1,r13
ldm14:	test	$0x4000,ssrc		;test bit zero
	je	ldm15
	mov	warm(r13),wr14
	add	$1,r13
ldm15:	test	$0x8000,ssrc		;test bit zero
	je	ldmfin
	mov	warm(r13),wr15
	mov	wpc,warmccr
	shr	$28,warmccr
	add	$1,r13
ldmfin:	mov	r13,wregs(dest)
	and	$0xFFFFFF,wpc	;BEGIN LOOP AGAIN
	mov	wpc,r1		; TOP OF INSTRUCTION FETCH LOOP
	mov	warm(r1),cond
	shr	$29,cond	;store conditionals
	mov	condTab(cond),rip
	
stm:	mov	wregs(dest),r13	;store original value of dest reg in r12
	and	$0xFFFFFF,r13

stm15:	test	$0x8000,ssrc
	je	stm14
	mov	warmccr,r14
	shl	$28,r14
	or	r14,r1
	sub	$1,r13
	mov	r1,warm(r13)
stm14:	test	$0x4000,ssrc
	je	stm13
	sub	$1,r13
	mov	wr14,warm(r13)
stm13:	test	$0x2000,ssrc
	je	stm12
	sub	$1,r13
	mov	wr13,warm(r13)
stm12:	test	$0x1000,ssrc
	je	stm11
	sub	$1,r13
	mov	wr12,warm(r13)
stm11:	test	$0x800,ssrc
	je	stm10
	sub	$1,r13
	mov	wr11,warm(r13)
stm10:	test	$0x400,ssrc
	je	stm9
	sub	$1,r13
	mov	wr10,warm(r13)
stm9:	test	$0x200,ssrc
	je	stm8
	sub	$1,r13
	mov	wr9,warm(r13)
stm8:	test	$0x100,ssrc
	je	stm7
	sub	$1,r13
	mov	wr8,warm(r13)
stm7:	test	$0x80,ssrc
	je	stm6
	sub	$1,r13
	mov	wr7,warm(r13)
stm6:	test	$0x40,ssrc
	je	stm5
	sub	$1,r13
	mov	wr6,warm(r13)
stm5:	test	$0x20,ssrc
	je	stm4
	sub	$1,r13
	mov	wr5,warm(r13)
stm4:	test	$0x10,ssrc
	je	stm3
	sub	$1,r13
	mov	wr4,warm(r13)
stm3:	test	$0x8,ssrc
	je	stm2
	sub	$1,r13
	mov	wr3,warm(r13)
stm2:	test	$4,ssrc
	je	stm1
	sub	$1,r13
	mov	wr2,warm(r13)
stm1:	test	$2,ssrc
	je	stm0
	sub	$1,r13
	mov	wr1,warm(r13)
stm0:	test	$1,ssrc
	je	stmfin
	sub	$1,r13
	mov	wr0,warm(r13)
stmfin:	mov	r13,wregs(dest)
	and	$0xFFFFFF,wpc	;BEGIN LOOP AGAIN
	mov	wpc,r1		; TOP OF INSTRUCTION FETCH LOOP
	mov	warm(r1),cond
	shr	$29,cond	;store conditionals
	mov	condTab(cond),rip

ldr:	lea 	0(ssrc,r11),r13
	and	$0xFFFFFF,r13
	mov	warm(r13),wregs(dest) ;r11 contains address
	and	$0xFFFFFF,wpc	      ;BEGIN LOOP AGAIN
	mov	wpc,r1		; TOP OF INSTRUCTION FETCH LOOP
	mov	warm(r1),cond
	shr	$29,cond	;store conditionals
	mov	condTab(cond),rip

ldrs:	lea	0(ssrc,r11),r13
	and	$0xFFFFFF,r13
	mov	warm(ssrc,r11),wregs(dest)
	add	$0,wregs(dest)		;update condition codes by calling add
	mov	ccr,warmccr
	and	$0xFFFFFF,wpc	;BEGIN LOOP AGAIN
	mov	wpc,r1		; TOP OF INSTRUCTION FETCH LOOP
	mov	warm(r1),cond
	shr	$29,cond	;store conditionals
	mov	condTab(cond),rip

str:	lea	0(ssrc,r11),r13
	and	$0xFFFFFF,r13
	mov	wregs(dest),warm(r13)	;r11 contains address
	and	$0xFFFFFF,wpc		;BEGIN LOOP AGAIN
	mov	wpc,r1		; TOP OF INSTRUCTION FETCH LOOP
	mov	warm(r1),cond
	shr	$29,cond	;store conditionals
	mov	condTab(cond),rip

strs:	lea	0(ssrc,r11),r13
	and	$0xFFFFFF,r13
	mov	wregs(dest),warm(ssrc,r11)
	add	$0,wregs(dest)		;update condition codes by calling add
	mov	ccr,warmccr
	and	$0xFFFFFF,wpc	;BEGIN LOOP AGAIN
	mov	wpc,r1		; TOP OF INSTRUCTION FETCH LOOP
	mov	warm(r1),cond
	shr	$29,cond	;store conditionals
	mov	condTab(cond),rip

stu:	lea	0(ssrc,r11),r13
	and	$0xFFFFFF,r13	;mask with 24
	add	$0,ssrc 	;reset condition codes based on offset
	cmovl	wregs(dest),warm(r13)	 ;if offset is negative
	cmovge	wregs(dest),warm(r11)	 ;if offset is positive
	mov	r13,wregs(fsrc)	;both cases address written to base register
	jmp	loop

stus:	lea	0(ssrc,r11),r13
	and	$0xFFFFFF,r13
	add	$0,ssrc
	cmovl	wregs(dest),warm(r13)
	cmovge	wregs(dest),warm(r11)
	mov	r13,wregs(fsrc)
	add	$0,wregs(dest)	;set ccr based off of value written to mem
	mov	ccr,warmccr
	and	$0xFFFFFF,wpc	;BEGIN LOOP AGAIN
	mov	wpc,r1		; TOP OF INSTRUCTION FETCH LOOP
	mov	warm(r1),cond
	shr	$29,cond	;store conditionals
	mov	condTab(cond),rip

ldu:	lea	0(ssrc,r11),r13
	and	$0xFFFFFF,r13
	add	$0,ssrc
	cmovl	warm(r13),wregs(dest) ;optimize by taking out r12!
	cmovge	warm(r11),wregs(dest)
	mov	r13,wregs(fsrc)
	and	$0xFFFFFF,wpc	;BEGIN LOOP AGAIN
	mov	wpc,r1		; TOP OF INSTRUCTION FETCH LOOP
	mov	warm(r1),cond
	shr	$29,cond	;store conditionals
	mov	condTab(cond),rip

ldus:	lea	0(ssrc,r11),r13
	and	$0xFFFFFF,r13
	add	$0,ssrc
	cmovl	warm(r13),wregs(dest)
	cmovge	warm(r11),wregs(dest)
	mov	r13,wregs(fsrc)
	cmp	$0,wregs(dest)
	mov	ccr,warmccr
	and	$0xFFFFFF,wpc	;BEGIN LOOP AGAIN
	mov	wpc,r1		; TOP OF INSTRUCTION FETCH LOOP
	mov	warm(r1),cond
	shr	$29,cond	;store conditionals
	mov	condTab(cond),rip

adr:	lea	0(ssrc,r11),wregs(dest)
	and	$0xFFFFFF,wregs(dest)
	and	$0xFFFFFF,wpc	;BEGIN LOOP AGAIN
	mov	wpc,r1		; TOP OF INSTRUCTION FETCH LOOP
	mov	warm(r1),cond
	shr	$29,cond	;store conditionals
	mov	condTab(cond),rip

b:	add	warm(r1),wpc		;add displacement to warm pc
	and	$0xFFFFFF,wpc		;BEGIN LOOP AGAIN
	mov	wpc,r1		; TOP OF INSTRUCTION FETCH LOOP
	mov	warm(r1),cond
	shr	$29,cond	;store conditionals
	mov	condTab(cond),rip

bl:	mov	warm(r1),ir
	add	$1,r1	     ;wpc already stored in r1
	add	ir,wpc     ;add displacement to warm pc
	mov	r1,wlr		;store next instruction in warm link reg.
	and	$0xFFFFFF,wpc	;BEGIN LOOP AGAIN
	mov	wpc,r1		; TOP OF INSTRUCTION FETCH LOOP
	mov	warm(r1),cond
	shr	$29,cond	;store conditionals
	mov	condTab(cond),rip
	
;;; DON'T WRITE ANY CODE BELOW THIS LINE
wregs:
wr0:	.data	0
wr1:	.data	0
wr2:	.data	0
wr3:	.data	0
wr4:	.data	0
wr5:	.data	0
wr6:	.data	0
wr7:	.data	0
wr8:	.data	0
wr9:	.data	0
wr10:	.data	0
wr11:	.data	0
wr12:	.data	0
wsp:	
wr13:	.data	0x00ffffff	
wlr:	
wr14:	.data	0
wpc:	
wr15:	.data	0	
	
warm:	
