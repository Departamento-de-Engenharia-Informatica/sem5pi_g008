export class NoMedicalRecordConditionsException extends Error {

    public readonly errorCode: number = 850;

    constructor() {
        super("No conditions found for this medical record.");
        this.name = "NoMedicalRecordConditionsException";
    }

    get code(): number {
        return this.errorCode;
    }
}
