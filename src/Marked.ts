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
    return {
      type: "left",
      value: e.message,
    };
  }
};
