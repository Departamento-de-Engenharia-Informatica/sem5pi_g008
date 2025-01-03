import { Code } from "../../../../src/domain/Shared/code";
import { AppError} from "../../../../src/domain/Shared/Exceptions/AppError";

describe("Code Value Object", () => {
  it("should create a valid Code object when a valid ICD-11 code is provided", () => {
    const validCode = "A01.0";
    const result = Code.create(validCode);
    expect(result.isSuccess).toBe(true);
    expect(result.getValue().value).toBe(validCode);
  });

  it("should trim whitespace and create a valid Code object", () => {
    const validCodeWithWhitespace = "   A01.0   ";
    const result = Code.create(validCodeWithWhitespace);
    expect(result.isSuccess).toBe(true);
    expect(result.getValue().value).toBe("A01.0");
  });

  it("should throw CODE_NULL_UNDEFINED if the input is null or undefined", () => {
    expect(() => Code.create(null as any)).toThrowError(AppError);
    expect(() => Code.create(undefined as any)).toThrowError(AppError);
    try {
      Code.create(null as any);
    } catch (error) {
      expect((error as AppError).code).toBe(801);
    }
  });

  it("should throw CODE_INVALID_WHITESPACE if the input is an empty string", () => {
    expect(() => Code.create("")).toThrowError(AppError);
    try {
      Code.create("");
    } catch (error) {
      expect((error as AppError).code).toBe(804);
    }
  });

  it("should throw INVALID_ICD11_CODE for invalid ICD-11 codes", () => {
    const invalidCodes = ["A", "123", "AA1234", "A12.", "A1.B2"];
    invalidCodes.forEach((invalidCode) => {
      expect(() => Code.create(invalidCode)).toThrowError(AppError);
      try {
        Code.create(invalidCode);
      } catch (error) {
        expect((error as AppError).code).toBe(805);
      }
    });
  });
});
