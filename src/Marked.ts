import { marked } from "marked";
import { FFI } from "../output/Marked";

const lexerImpl = (str: string) => {
  try {
    const value = marked.lexer(str);
    console.log(value);
    return {
      type: "success",
      value,
    } as const;
  } catch (e) {
    return {
      type: "failure",
      value: e.message,
    } as const;
  }
};

export const ffi : FFI = { lexerImpl };
