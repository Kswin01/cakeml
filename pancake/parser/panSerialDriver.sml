open HolKernel Parse boolLib bossLib stringLib numLib intLib;
open preamble panPtreeConversionTheory;

val _ = new_theory "panConcreteExamples";

(** 
The first section of this file is taken from Craig McLaughlin's panConcreteExamples.sml file.
*)

local
  val f =
    List.mapPartial
       (fn s => case remove_whitespace s of "" => NONE | x => SOME x) o
    String.tokens (fn c => c = #"\n")
in
  fun quote_to_strings q =
    f (Portable.quote_to_string (fn _ => raise General.Bind) q)
end

fun lex_pancake q =
  let
    val code = quote_to_strings q |> String.concatWith "\n" |> fromMLstring
  in
    EVAL “pancake_lex ^code”
end

fun parse_tree_pancake q =
  let
    val code = quote_to_strings q |> String.concatWith "\n" |> fromMLstring
  in
    EVAL “parse (pancake_lex ^code)”
end

(** Copied from panPtreeConversion *)
fun parse_pancake q =
  let
    val code = quote_to_strings q |> String.concatWith "\n" |> fromMLstring
  in
    EVAL “parse_to_ast ^code”
  end

(** 
Exploratory functions for an sDDF compliant serial driver for the imx8mm board.
*)


val uart_drv_putchar = '#UART_REG(x y k z);
                        if k & TX_EMPTY {
                          #UART_REG(x y k z);
                        }';

val treePutchar = parse_pancake uart_drv_putchar;


(** This is a very broken copy of the putchar function in serial.c (serial sDDF). 
Need to figure out here the memory management works as well as how functions are called. *)
val uart_sddf_putchar = ‘var c_arr = @base {
                          var clen = 1 {
                            var a_arr = @base + 32 {
                              var a_len = 0 {
                                var temp_c = @base + 64 {
                                  var temp_clen = 0 {
                                    var temp_a = @base + 96 {
                                      var temp_alen = 0 {
                                       #internal_is_tx_fifo_busy(temp_c temp_clen temp_a temp_alen);
                                        if temp_a == 1 {
                                           return -1;
                                        } else {
                                           if c == 10 {
                                              strb c_arr, 13;
                                              #putchar_regs(c_arr clen a_arr alen);
                                           }
                                           while 1 == 1 {
                                                 #internal_is_tx_fifo_busy(temp_c temp_clen temp_a temp_alen);
                                                 if temp_a == 0 {
                                                    break;
                                                 }
                                           }
                                        }
                                        strb c_arr, c;
                                        #putchar_regs(c_arr clen a_arr alen);
                                      }
                                    }
                                  }
                                }
                              }
                            }
                          }
                          }’;

val treeSDDFPutchar = parse_pancake uart_sddf_putchar;

val rawTx = ‘var i = 0 {
                while i < len {
                    temp = ldb phys;
                    if temp < 0 {
                        break;
                    }
                    uart_sddf_putchar(temp);
                    i = i + 1;
                    phys = phys + 1;
                }
}
’;

val treeRawTx = parse_pancake rawTx;

val handleTx = ‘var c_arr = @base {
                    var c_len = 0 {
                        var a_arr = @base + 32 {
                            var a_len = 2048 {
                                while 1 == 1 {
                                    #serial_driver_dequeue_used(c_arr c_len a_arr a_len);
                                    ret = ldb c_arr;
                                    if ret == 0 {
                                      return -1;
                                    }

                                    rawTx(a_arr, a_len);

                                    strb c_arr, 1;
                                    c_len = 1;
                                    a_len = 0;
                                    #serial_enqueue_avail(c_arr c_len a_arr a_len);

                                    ret2 = ldb a_arr;
                                    if ret2 <> 0 {
                                      return - 1;
                                    }
                                }
                            }
                        }
                    }
}
’;

val treeHandleTx = parse_pancake handleTx;

val handleIRQ = ‘var getchar_c = @base {
                  var getchar_clen = 1 {
                    var getchar_a = @base + 1 {
                      var getchar_alen = 0 {
                        #getchar(getchar_c getchar_clen getchar_a getchar_alen);
                        got_char = ldb getchar_a;
                        if got_char <> 0 {
                          return -1;
                        } else {
                          while 1 == 1 {
                            var c_arr = @base + 2 {
                              var c_len = 1 {
                                var a_arr = @base + 3 {
                                  var a_len = 1 {
                                    strb c_arr, 0;
                                    strb a_arr, 0;
                                    #serial_dequeue_avail(c_arr c_len a_arr a_len);
                                    dequeue_ret = ldb a_arr;
                                    if dequeue_ret == -1 {
                                      break;
                                    } 
                                    if dequeue_ret <> 0 {
                                      return -1;
                                    }

                                    var enqueue_c_arr = @base + 5 {
                                      var enqueue_clen = 2 {
                                        var enqueue_a_arr = @base + 6 {
                                          var enqueue_alen = a_len {
                                            strb enqueue_c_arr, 0;
                                            strb enqueue_c_arr + 1, got_char;
                                            strb enqueue_a_arr, -1;
                                            #serial_enqueue_used(enqueue_c_arr enqueue_clen enqueue_a_arr enqueue_alen);
                                            enqueue_ret = ldb a_arr;
                                            if enqueue_ret <> 0 {
                                              return -1;
                                            }
                                          }
                                        }
                                      }
                                    }
                                  }
                                }
                              }
                            }
                          }
                        }
                      }
                    }
                  }
}’;

val treeHandleIRQ = parse_pancake handleIRQ;


(** 
This is currently going to be offloaded to the ffi file. We can only have one entry point
into our pancake program and so we will only have the main with a switch case similair to 
the current notified implementation. There is no point having the init function come into the 
pancake program just to make an ffi call back to the c file.
*)
val init = ‘var c_arr = @base {
            var clen = 0 {
              var a_arr = @base + 32 {
                var alen = 0 {
                  strb a_arr, -1;
                  #init_post(c_arr clen a_arr alen);
                  init_post_ret = ldb a_arr;
                  if init_post_ret <> 0 {
                    return -1;
                  }
                }
              } 
            }
}’;

val treeInit = parse_pancake init;

val handleRx = ‘var c_arr = @base {
            var clen = 0 {
              var a_arr = @base + 1 {
                var alen = 0 {
                  #increment_num_chars(c_arr clen a_arr alen);
                  increment_ret = ldb a_arr;
                  if increment_ret <> 0 {
                    return -1;
                  }
                }
              } 
            }
}’;

val treeHandleRx = parse_pancake handleRx;


(** 
The main function will be our entry point into our pancake program. This will be called from 
out ffi notified function whenever a notification comes in for our driver. This is because
we can only have one entry point into our pancake program, and thus only one main function.
This main function should have one argument, and that is the channel that notified us.

If we have an IRQ, we will need to acknowledge on return to the notified function
*)
val main = ‘
  if ch == 1 {
    handle_irq();
    return;
  }

  if ch == 8 {
    handle_tx();
  }

  if ch == 10 {
    handle_rx();
  }

’;

val treeNotified = parse_pancake notified;

val addrTest = ‘if 2 >= 1 { x = 2; }’;

val treeAddrTest = parse_pancake addrTest;

val _ = export_theory();
