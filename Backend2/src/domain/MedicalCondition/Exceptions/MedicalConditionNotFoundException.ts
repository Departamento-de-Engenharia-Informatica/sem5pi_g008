export class MedicalConditionNotFoundException extends Error {

    public readonly errorCode: number = 810;

    constructor(message : string) {
        super(message);
        this.name = "MedicalConditionNotFoundException";
    }

    get code(): number {
        return this.errorCode;
    }
}
