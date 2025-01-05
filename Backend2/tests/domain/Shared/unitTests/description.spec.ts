import {Description} from "../../../../src/domain/Shared/description";
import {AppError} from "../../../../src/domain/Shared/Exceptions/AppError";


describe("Description Value Object", () => {
  it("should create a valid Description object when a valid input is provided", () => {
    const validDescription = "This is a valid description.";
    const result = Description.create(validDescription);
    expect(result.isSuccess).toBe(true);
    expect(result.getValue().value).toBe(validDescription);
  });

  it("should throw DESCRIPTION_NULL_UNDEFINED if the input is null or undefined", () => {
    expect(() => Description.create(null as any)).toThrowError(AppError);
    expect(() => Description.create(undefined as any)).toThrowError(AppError);
    try {
      Description.create(null as any);
    } catch (error) {
      expect((error as AppError).code).toBe(802);
    }
  });

  it("should throw DESCRIPTION_INVALID_LENGTH if the input exceeds 2048 characters", () => {
    const longDescription = "a".repeat(2049); // 2049 characters
    expect(() => Description.create(longDescription)).toThrowError(AppError);
    try {
      Description.create(longDescription);
    } catch (error) {
      expect((error as AppError).code).toBe(809);
    }
  });

  it("should allow a description with exactly 2048 characters", () => {
    const maxDescription = "a".repeat(2048); // Exactly 2048 characters
    const result = Description.create(maxDescription);
    expect(result.isSuccess).toBe(true);
    expect(result.getValue().value).toBe(maxDescription);
  });

  it("should allow an empty description string", () => {
    const emptyDescription = "";
    const result = Description.create(emptyDescription);
    expect(result.isSuccess).toBe(true);
    expect(result.getValue().value).toBe(emptyDescription);
  });

  it("should preserve leading and trailing whitespace in the description", () => {
    const descriptionWithWhitespace = "   Valid description with spaces   ";
    const result = Description.create(descriptionWithWhitespace);
    expect(result.isSuccess).toBe(true);
    expect(result.getValue().value).toBe(descriptionWithWhitespace);
  });
});
