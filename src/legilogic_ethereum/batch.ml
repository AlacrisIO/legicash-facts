open Legilogic_lib
open Signing

open Assembly

let batch_contract owner =
  assemble [
      eGETPC; (* at instruction 0, so push 0 on stack while it's cheap! -- 0 *)
      (* check that the caller is the contract's owner *)
      ePUSH20; string (Address.to_big_endian_bits owner);
      eCALLER; eEQ; ePUSH1; fixup 1 (Label "loop_init") 30; eJUMPI;
      eDUP1; eDUP1; eREVERT; (*abort*)
      label "loop_init"; (* @ 30 -- 0 *)
      push_int 1; push_int 96; push_int 2; eEXP; eDUP3; eDUP2; eSUB; eCALLDATASIZE; eDUP5;
      (* -- 0 1 2**96 2**96-1 datasize 0 *)
      ePUSH1; fixup 1 (Label "loop_entry") 48; eJUMP;
      label "loop"; (* @ 45 *)
      push_int 32; eADD;
      label "loop_entry"; (* @ 48 -- 0 1 2**96 2**96-1 datasize current_index *)
      eDUP1; eDUP3; eSUB; eLT; (* -- 0 1 2**96 2**96-1 datasize current_index datasize-current_index *)
      ePUSH1; fixup 1 (Label "loop_body") 58; eJUMPI; (* if less then loop_body, else return *)
      eDUP3; eDUP1; eRETURN; (* 0 1 2**96 2**96-1 datasize current_index -- return *)
      label "loop_body"; (* @ 58 -- 0 1 2**96 2**96-1 datasize current_index *)
      eDUP6; eDUP1; eDUP1; eDUP1; eDUP5; eCALLDATALOAD; eDUP1; eDUP9; eAND;
      (* -- 0 1 2**96 2**96-1 datasize current_index 0 0 0 0 data value *)
      eSWAP1; eDUP10; eSWAP1; eDIV; eGAS;
      (* -- 0 1 2**96 2**96-1 datasize current_index 0 0 0 0 value address gas *)
      eCALL; ePUSH1; fixup 1 (Label "loop") 45; eJUMPI; (* transfer, loop if successful *)
      (* 0 1 2**96 2**96-1 datasize current_index -- *)
      eDUP6; eDUP1; eREVERT]

let batch_contract_init owner =
  let runtime = batch_contract owner in
  assemble [
    eGETPC; (* cheap 0 *)
    push_int (String.length runtime); eDUP2; eCALLVALUE;
    eDUP3; ePUSH1; fixup 1 (Label "runtime_start") 13; eDUP4; eCODECOPY; eCREATE; eDUP1; eRETURN;
    label "runtime_start"; (* @ 13 *)
    string runtime]

let foo = batch_contract_init (Address.of_0x "0x0123456789012345678901234567890123456789")
