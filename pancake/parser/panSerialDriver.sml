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
                            var a_arr = @base + 1 {
                              var a_len = 1 {
                                var temp_c = @base + 2 {
                                  var temp_clen = 1 {
                                    var temp_a = @base + 3 {
                                      var temp_alen = 1 {
                                       #internal_is_tx_fifo_busy(temp_c temp_clen temp_a temp_alen);
                                       var tx_fifo_ret  = 0{
                                       tx_fifo_ret = ldb temp_a;
                                          if tx_fifo_ret == 1 {
                                            return -1;
                                          } else {
                                            if c == 10 {
                                                strb c_arr, 13;
                                                #putchar_regs(c_arr clen a_arr alen);
                                            }
                                            while 1 == 1 {
                                                  #internal_is_tx_fifo_busy(temp_c temp_clen temp_a temp_alen);
                                                  
                                                  tx_fifo_ret = ldb temp_a;
                                                  if tx_fifo_ret <> 1 {
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
                          }
                          return 0;
                          }’;

val treeSDDFPutchar = parse_pancake uart_sddf_putchar;

val rawTx = ‘var i = 0 {
              var temp = 0{
                while i < len {
                    temp = ldb phys;
                    if temp < 0 {
                        break;
                    }
                    var ret = 1 {
                      while ret <> 0 {
                        ret = uart_sddf_putchar(temp);
                      }
                    }

                    i = i + 1;
                    phys = phys + 1;
                }
              }
            }
’;

val treeRawTx = parse_pancake rawTx;

val handleTx = ‘var c_arr = @base {
                    var c_len = 1 {
                        strb c_arr, 1;
                        var a_arr = @base + 32 {
                            var a_len = 2048 {
                                while 1 == 1 {
                                  var ret = 0 {
                                    #serial_driver_dequeue_used(c_arr c_len a_arr a_len);
                                    ret = ldb c_arr;
                                    if ret == 0 {
                                      return -1;
                                    }
                                  }
                                  
                                  var buff_len = 0 {

                                    //buff_len = ((ldb c + 1) << 56) | ((ldb c + 2) << 48) | ((ldb c + 3) << 40) | ((ldb c + 4) << 32) | ((ldb c + 5) << 24) | ((ldb c + 6) << 16) | ((ldb c + 7) << 8) | (ldb c + 8);

                                    rawTx(a_arr, 1);

                                  }
                                  strb c_arr, 1;
                                  c_len = 1;
                                  #serial_enqueue_avail(c_arr c_len a_arr a_len);

                                  var ret2 = 0 {
                                    ret2 = ldb a_arr;
                                    if ret2 <> 0 {
                                      return -1;
                                    }
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
                                    var dequeue_ret = 0 {
                                    dequeue_ret = ldb a_arr;
                                      if dequeue_ret <> 0 {
                                        return -1;
                                      }
                                    }
                                    var enqueue_c_arr = @base + 32 {
                                      var enqueue_clen = 2 {
                                        var enqueue_a_arr = @base + 34 {
                                          var enqueue_alen = a_len {
                                            strb enqueue_c_arr, 0;
                                            strb enqueue_c_arr + 1, got_char;
                                            #serial_enqueue_used(enqueue_c_arr enqueue_clen a_arr enqueue_alen);
                                            var enqueue_ret = 0 {
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
                  var increment_ret = 0 {
                    increment_ret = ldb a_arr;
                    if increment_ret <> 0 {
                      return -1;
                    }
                  }
                }
              } 
            }
}’;

val treeHandleRx = parse_pancake handleRx;


(** 
The main function will be our entry point into our pancake program. This will be called from 
our ffi notified function whenever a notification comes in for our driver.
This main function should have one argument, and that is the channel that notified us.

If we have an IRQ, we will need to acknowledge on successful return to the notified function.
*)
val main = ‘
  if ch == 1 {
    var getchar_c = @base {
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
                        var dequeue_ret = 0 {
                        dequeue_ret = ldb a_arr;
                          if dequeue_ret <> 0 {
                            return -1;
                          }
                        }
                        var enqueue_c_arr = @base + 32 {
                          var enqueue_clen = 2 {
                            var enqueue_a_arr = @base + 34 {
                              var enqueue_alen = a_len {
                                strb enqueue_c_arr, 0;
                                strb enqueue_c_arr + 1, got_char;
                                #serial_enqueue_used(enqueue_c_arr enqueue_clen a_arr enqueue_alen);
                                var enqueue_ret = 0 {
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
      }
    }
    return 1;
  }

  if ch == 8 {
    var c_arr = @base {
      var c_len = 1 {
          strb c_arr, 1;
          var a_arr = @base + 32 {
              var a_len = 2048 {
                  while 1 == 1 {
                    var ret = 0 {
                      #serial_driver_dequeue_used(c_arr c_len a_arr a_len);
                      ret = ldb c_arr;
                      if ret == 0 {
                        return -1;
                      }
                    }
                    
                    var buff_len = 0 {

                      //buff_len = ((ldb c + 1) << 56) | ((ldb c + 2) << 48) | ((ldb c + 3) << 40) | ((ldb c + 4) << 32) | ((ldb c + 5) << 24) | ((ldb c + 6) << 16) | ((ldb c + 7) << 8) | (ldb c + 8);

                    var i = 0 {
                      var temp = 0 {
                        while i < len {
                            temp = ldb phys;
                            if temp < 0 {
                                break;
                            }
                            var ret = 1 {
                              while ret <> 0 {
                                var c_arr_uart = @base {
                                  var clen_uart = 1 {
                                    var a_arr_uart = @base + 1 {
                                      var a_len_uart = 1 {
                                        var temp_c_uart = @base + 2 {
                                          var temp_clen_uart = 1 {
                                            var temp_a_uart = @base + 3 {
                                              var temp_alen_uart = 1 {
                                              #internal_is_tx_fifo_busy(temp_c_uart temp_clen_uart temp_a_uart temp_alen_uart);
                                              var tx_fifo_ret  = 0 {
                                              tx_fifo_ret = ldb temp_a_uart;
                                                  if tx_fifo_ret == 1 {
                                                    return -1;
                                                  } else {
                                                    if temp == 10 {
                                                        strb c_arr_uart, 13;
                                                        #putchar_regs(c_arr_uart clen_uart a_arr_uart alen_uart);
                                                    }
                                                    while 1 == 1 {
                                                          #internal_is_tx_fifo_busy(temp_c_uart temp_clen_uart temp_a_uart temp_alen_uart);
                                                          
                                                          tx_fifo_ret = ldb temp_a_uart;
                                                          if tx_fifo_ret <> 1 {
                                                              break;
                                                          }
                                                    }
                                                  }
                                                  strb c_arr_uart, temp;
                                                  #putchar_regs(c_arr_uart clen_uart a_arr_uart alen_uart);
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

                            i = i + 1;
                            phys = phys + 1;
                        }
                      }
                    }

                    }
                    strb c_arr, 1;
                    c_len = 1;
                    #serial_enqueue_avail(c_arr c_len a_arr a_len);

                    var ret2 = 0 {
                      ret2 = ldb a_arr;
                      if ret2 <> 0 {
                        return -1;
                      }
                    }
                  }
              }
          }
      }
    }
    return 1;
  }

  if ch == 10 {
    var c_arr = @base {
      var clen = 0 {
        var a_arr = @base + 1 {
          var alen = 0 {
            #increment_num_chars(c_arr clen a_arr alen);
            var increment_ret = 0 {
              increment_ret = ldb a_arr;
              if increment_ret <> 0 {
                return -1;
              }
            }
          }
        } 
      }
    }
    return 1;
  }

’;

val treeMain = parse_pancake main;

val addrTest = ‘if 2 >= 1 { x = 2; }’;

val treeAddrTest = parse_pancake addrTest;

val _ = export_theory();