const ErrorCodes = {
    CODE_NULL_UNDEFINED: { code: 801, message: "Code can not be null or undefined." },
    DESCRIPTION_NULL_UNDEFINED: { code: 802, message: "Description can not be null or undefined." },
    DESIGNATION_NULL_UNDEFINED: { code: 803, message: "Designation can not be null or undefined." },
    
    CODE_INVALID_WHITESPACE: { code: 804, message: "Code can not be empty." },
    INVALID_ICD11_CODE: { code: 805, message: "Invalid ICD-11 code." },
    
    DESIGNATION_INVALID_WHITESPACE: { code: 806, message: "Designation can not be empty." },
    DESIGNATION_INVALID_LENGTH: { code: 807, message: "Designation must have a maximum of 100 characters." },
    
    DESCRIPTION_INVALID_WHITESPACE: { code: 808, message: "Description can not be empty." },
    DESCRIPTION_INVALID_LENGTH: { code: 809, message: "Description must have a maximum of 2048 characters." },

};

export class AppError extends Error {

    public readonly errorCode: number;

    get code(): number {
        return this.errorCode;
    }
    
    constructor(errorType: keyof typeof ErrorCodes) {
        
        super(ErrorCodes[errorType].message); 
        this.name = "AppError";
        this.errorCode = ErrorCodes[errorType].code;
        Object.setPrototypeOf(this, AppError.prototype);
    }
    
}
