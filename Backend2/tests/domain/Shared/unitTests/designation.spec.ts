import {Designation} from "../../../../src/domain/Shared/designation";
import {AppError} from "../../../../src/domain/Shared/Exceptions/AppError";


describe("Designation Value Object", () => {
  it("should create a valid Designation object when a valid input is provided", () => {
    const validDesignation = "Software Engineer";
    const result = Designation.create(validDesignation);
    expect(result.isSuccess).toBe(true);
    expect(result.getValue().value).toBe(validDesignation);
  });

  it("should trim whitespace and create a valid Designation object", () => {
    const validDesignationWithWhitespace = "   Software Engineer   ";
    const result = Designation.create(validDesignationWithWhitespace);
    expect(result.isSuccess).toBe(true);
    expect(result.getValue().value).toBe("Software Engineer");
  });

  it("should throw DESIGNATION_NULL_UNDEFINED if the input is null or undefined", () => {
    expect(() => Designation.create(null as any)).toThrowError(AppError);
    expect(() => Designation.create(undefined as any)).toThrowError(AppError);
    try {
      Designation.create(null as any);
    } catch (error) {
      expect((error as AppError).code).toBe(803);
    }
  });

  it("should throw DESIGNATION_INVALID_WHITESPACE if the input is an empty string", () => {
    expect(() => Designation.create("")).toThrowError(AppError);
    try {
      Designation.create("");
    } catch (error) {
      expect((error as AppError).code).toBe(806);
    }
  });

  it("should throw DESIGNATION_INVALID_WHITESPACE if the input is only whitespace", () => {
    expect(() => Designation.create("   ")).toThrowError(AppError);
    try {
      Designation.create("   ");
    } catch (error) {
      expect((error as AppError).code).toBe(806);
    }
  });

  it("should throw DESIGNATION_INVALID_LENGTH if the input exceeds 100 characters", () => {
    const longDesignation = "a".repeat(101);
    expect(() => Designation.create(longDesignation)).toThrowError(AppError);
    try {
      Designation.create(longDesignation);
    } catch (error) {
      expect((error as AppError).code).toBe(807);
    }
  });

  it("should allow exactly 100 characters", () => {
    const validDesignation = "a".repeat(100);
    const result = Designation.create(validDesignation);
    expect(result.isSuccess).toBe(true);
    expect(result.getValue().value).toBe(validDesignation);
  });
});
