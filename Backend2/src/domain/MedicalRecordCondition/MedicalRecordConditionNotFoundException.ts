export class MedicalRecordConditionNotFoundException extends Error {

    public readonly errorCode: number = 851;

    constructor(message: string) {
        super(message);
        this.name = "MedicalRecordConditionNotFoundException";
    }

    get code(): number {
        return this.errorCode;
    }
}