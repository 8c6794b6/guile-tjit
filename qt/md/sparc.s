/* sparc.s -- assembly support for the `qt' thread building kit. */

/*
 * QuickThreads -- Threads-building toolkit.
 * Copyright (c) 1993 by David Keppel
 *
 * Permission to use, copy, modify and distribute this software and
 * its documentation for any purpose and without fee is hereby
 * granted, provided that the above copyright notice and this notice
 * appear in all copies.  This software is provided as a
 * proof-of-concept and for demonstration purposes; there is no
 * representation about the suitability of this software for any
 * purpose.
 */

/* #include <machine/trap.h> */

	.text
	.align 4
	.global qt_blocki
	.global qt_block
	.global qt_abort
	.global qt_start
	.global qt_vstart

/* Register assignment:
// %o0:	incoming `helper' function to call after cswap
//	also used as outgoing sp of old thread (qt_t *)
// %o1, %o2:
//	parameters to `helper' function called after cswap
// %o3:	sp of new thread
// %o5: tmp used to save old thread sp, while using %o0
//	to call `helper' f() after cswap.
//
//
// Aborting a thread is easy if there are no cached register window
// frames: just switch to the new stack and away we go.  If there are
// cached register window frames they must all be written back to the
// old stack before we move to the new stack.  If we fail to do the
// writeback then the old stack memory can be written with register
// window contents e.g., after the stack memory has been freed and
// reused.  
//
// If you don't believe this, try setting the frame pointer to zero
// once we're on the new stack.  This will not affect correctnes
// otherwise because the frame pointer will eventually get reloaded w/
// the new thread's frame pointer.  But it will be zero briefly before
// the reload.  You will eventually (100,000 cswaps later on a small
// SPARC machine that I tried) get an illegal instruction trap  from
// the kernel trying to flush a cached window to location 0x0.
//
// Solution: flush windows before switching stacks, which invalidates
// all the other register windows.  We could do the trap
// conditionally: if we're in the lowest frame of a thread, the fp is
// zero already so we know there's nothing cached.  But we expect most
// aborts will be done from a first function that does a `save', so we
// will rarely save anything and always pay the cost of testing to see
// if we should flush.
//
// All floating-point registers are caller-save, so this routine
// doesn't need to do anything to save and restore them.
//
// `qt_block' and `qt_blocki' return the same value as the value
// returned by the helper function.  We get this ``for free''
// since we don't touch the return value register between the
// return from the helper function and return from qt_block{,i}.
*/

qt_block:
qt_blocki:
	sub %sp, 8, %sp		/* Allocate save area for return pc. */
	st %o7, [%sp+64]	/* Save return pc. */
qt_abort:
	ta 0x03			/* Save locals and ins. */
	mov %sp, %o5		/* Remember old sp w/o chng ins/locals. */
	sub %o3, 64, %sp	/* Allocate kwsa, switch stacks. */
	call %o0, 0		/* Call `helper' routine. */
	mov %o5, %o0		/* Pass old thread to qt_after_t() */
				/* .. along w/ args in %o1 & %o2. */

	/* Restore callee-save regs.  The kwsa
	// is on this stack, so offset all
	// loads by sizeof(kwsa), 64 bytes.
	*/
	ldd [%sp+ 0+64], %l0
	ldd [%sp+ 8+64], %l2
	ldd [%sp+16+64], %l4
	ldd [%sp+24+64], %l6
	ldd [%sp+32+64], %i0
	ldd [%sp+40+64], %i2
	ldd [%sp+48+64], %i4
	ldd [%sp+56+64], %i6
	ld [%sp+64+64], %o7	/* Restore return pc. */

	retl			/* Return to address in %o7. */
	add %sp, 72, %sp	/* Deallocate kwsa, ret pc area. */


/* The function calling conventions say there has to be a 1-word area
// in the caller's stack to hold a pointer to space for aggregate
// return values.  It also says there should be a 6-word area to hold
// %o0..%o5 if the callee wants to save them (why?  I don't know...)
// Round up to 8 words to maintain alignment.
//
// Parameter values were stored in callee-save regs and are moved to
// the parameter registers.
*/
qt_start:
	mov %i1, %o0		/* `pu': Set up args to `only'. */
	mov %i2, %o1		/* `pt'. */
	mov %i4, %o2		/* `userf'. */
	call %i5, 0		/* Call client function. */
	sub %sp, 32, %sp	/* Allocate 6-word callee space. */

	call qt_error, 0	/* `only' erroniously returned. */
	nop


/* Same comments as `qt_start' about allocating rounded-up 7-word
// save areas. */

qt_vstart:
	sub %sp, 32, %sp	/* Allocate 7-word callee space. */
	call %i5, 0		/* call `startup'. */
	mov %i2, %o0		/* .. with argument `pt'. */

	add %sp, 32, %sp	/* Use 7-word space in varargs. */
	ld [%sp+ 4+64], %o0	/* Load arg0 ... */
	ld [%sp+ 8+64], %o1
	ld [%sp+12+64], %o2
	ld [%sp+16+64], %o3
	ld [%sp+20+64], %o4
	call %i4, 0		/* Call `userf'. */
	ld [%sp+24+64], %o5

				/* Use 6-word space in varargs. */
	mov %o0, %o1		/* Pass return value from userf */
	call %i3, 0		/* .. when call `cleanup. */
	mov %i2, %o0		/* .. along with argument `pt'. */

	call qt_error, 0	/* `cleanup' erroniously returned. */
	nop
