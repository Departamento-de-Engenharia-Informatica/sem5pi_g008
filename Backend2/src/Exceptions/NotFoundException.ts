export class NotFoundException extends Error {

  public readonly errorCode: number;

  get code(): number {
    return this.errorCode;
  }

  constructor(errorMsg: string) {

    super(errorMsg || "Resource not found");
    this.name = "NotFoundException";
    this.errorCode = 404;
    Object.setPrototypeOf(this, NotFoundException.prototype);
  }

}
