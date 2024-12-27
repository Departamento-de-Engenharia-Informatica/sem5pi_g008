export class NoMedicalRecordException extends Error {

    public readonly errorCode: number = 900;

    constructor() {
        super("No medical record found.");
        this.name = "NoMedicalRecordException";
    }

    get code(): number {
        return this.errorCode;
    }
}
