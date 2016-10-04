format MZ
entry main:start

;GDT selectors for protected mode.
;TI=0 (GDT), RPL=00b
CODE_SELECTOR       = 1000b
STACK_SELECTOR      = 10000b
DATA_SELECTOR       = 11000b
VIDEOMEM_SELECTOR   = 100000b
RM_CODE_SELECTOR    = 101000b
RM_DATA_SELECTOR    = 110000b

segment main use16
set_descriptor:
;eax=segment address
;ds:bx=descriptor address
;cl=access rights byte
;edx=limit

    push cx             ;save cx value for further use
    mov cx,ax           ;cx=2 lower bytes of address
    shl ecx,16          ;move cx to upper bytes of ecx
    mov cx,dx           ;cx=0..15 bits of limit
    mov [bx],ecx        ;save 4 lower bytes of descriptor
    shr eax,16          ;ax=2 higher bytes of address
    mov ch,ah           ;ch=4th byte of address
    shr edx,16          ;dl=0000+16..19 bits of limit
    mov cl,dl
    shl ecx,16          ;move cx to upper bytes of ecx
    mov cl,al           ;cl=3rd byte of address
    pop ax              ;al=access rights byte
    mov ch,al
    mov [bx+4],ecx      ;save 4 higher bytes of descriptor
    add bx,8            ;point at next descriptor
    ret

print_string:
;ds:bx=string address
;es:di=output address

    mov ah,10000100b    ;define text and background color
    put_char:
        mov al,[bx]
        inc bx
        test al,al
        jz str_end
        mov [es:di],ax
        add di,2
        jmp put_char
    str_end:
        ret

start:
    mov ax,main
    mov ds,ax

;save code segment and back_to_rm label value to restore from protected mode
    mov [rm_segment],cs
    lea ax,[back_to_rm]
    mov [rm_offset],ax

;save register values
    mov [rm_ss],ss
    mov [rm_ds],ds
    mov [rm_es],es
    mov [rm_fs],fs
    mov [rm_gs],gs

    xor eax,eax
    mov ax,cs
    shl eax,4           ;eax=physical address of main segment
    push eax
    lea bx,[GDT+8]      ;skip null descriptor
    mov edx,1024        ;set limit to 1024 bytes
    mov cl,10011000b    ;P=1, DPL=00b, S=1, Type=100b, A=0
    call set_descriptor ;set code segment descriptor

    pop eax
    push eax
    xor edx,edx
    lea dx,[stack_start]
    add eax,edx         ;eax=physical address of stack beginning
    mov edx,1024        ;set limit to 1024 bytes
    mov cl,10010110b    ;P=1, DPL=00b, S=1, Type=011b, A=0
    call set_descriptor ;set stack segment descriptor

    pop eax
    push eax
    xor edx,edx
    xor ecx,ecx
    lea dx,[data_end]
    lea cx,[data_start]
    add eax,ecx
    sub dx,cx           ;compute limit of data segment
    mov cl,10010010b    ;P=1, DPL=00b, S=1, Type=001b, A=0
    call set_descriptor ;set data segment descriptor

    mov eax,0b8000h     ;video memory address
    mov edx,4000        ;video memory segment limit
    mov cl,10010010b    ;P=1, DPL=00b, S=1, Type=001b, A=0
    call set_descriptor ;set video memory segment

    pop eax
    push eax
    mov edx,0FFFFh
    mov cl,10011010b    ;P=1, DPL=00b, S=1, Type=101b, A=0
    call set_descriptor ;set r-mode code descriptor

    pop eax
    push eax
    mov edx,0FFFFh
    mov cl,10010010b    ;P=1, DPL=00b, S=1, Type=001b, A=0
    call set_descriptor ;set r-mode data descriptor

    mov [GDT_limit],55  ;limit of GDT
    pop eax
    xor edx,edx
    lea dx,[GDT]
    add eax,edx
    mov [GDT_address],eax

    ;clear the screen
    mov ax,3
    int 10h

    cli
    lgdt fword [GDTR]

;store stack pointer just before switching to protected mode
    mov [rm_sp],sp

;enter protected mode
    mov eax,cr0
    or al,1             ;set PE flag to 1
    mov cr0,eax

    db 0eah             ;far jump opcode
    dw start_pm
    dw CODE_SELECTOR

start_pm:
;initialize segment selectors
    mov ax,STACK_SELECTOR
    mov ss,ax
    mov sp,0

    mov ax,DATA_SELECTOR
    mov ds,ax

    mov ax,VIDEOMEM_SELECTOR
    mov es,ax

    xor bx,bx
    mov di,72           ;text start position
    call print_string

prepare_for_rm:
    mov ax,RM_DATA_SELECTOR
    mov ss,ax
    mov ds,ax
    mov es,ax
    mov fs,ax
    mov gs,ax

    mov eax,cr0
    and al,11111110b    ;set PE flag to 0
    mov cr0,eax

    db 0eah             ;far jump opcode
    rm_offset dw ?      ;will be filled before
    rm_segment dw ?     ;entering protected mode

back_to_rm:
;restore registers' state
    mov ss,[rm_ss]
    mov ds,[rm_ds]
    mov es,[rm_es]
    mov fs,[rm_fs]
    mov gs,[rm_gs]

;restore stack pointer just before enabling interrupts
    mov sp,[rm_sp]
    sti

;exit
    mov ax,4c00h
    int 21h

;--------------------------------------------------------------------
;registers' state before entering protected mode
rm_ss dw ?
rm_sp dw ?
rm_ds dw ?
rm_es dw ?
rm_fs dw ?
rm_gs dw ?

;GDTR register image
GDTR:
    GDT_limit dw ?
    GDT_address dd ?
GDT:
    db 8 dup(?);null descriptor
    db 8 dup(?);code segment descriptor
    db 8 dup(?);stack segment descriptor
    db 8 dup(?);data segment descriptor
    db 8 dup(?);video memory segment descriptor
    db 8 dup(?);r-mode code segment descriptor
    db 8 dup(?);r-mode data segment descriptor

;--------------------------------------------------------------------
;protected mode data segment
data_start:
    db "P-Mode!",0
data_end:

;--------------------------------------------------------------------
;protected mode stack segment
    db 1024 dup(?);place reserved for stack
stack_start:
