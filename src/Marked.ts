import { marked } from "marked";
import { LexerImpl } from "../output/Marked";

const lexerImpl: LexerImpl = (str) => {
  try {
    const value = marked.lexer(str);

    return {
      type: "right",
      value,
    };
  } catch (e) {
    const message = e instanceof Error ? e.message : "unknown error";
    
    return {
      type: "left",
      value: message,
    };
  }
};
