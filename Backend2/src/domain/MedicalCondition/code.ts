import {ValueObject} from "../../core/domain/ValueObject";
import {Result} from "../../core/logic/Result";
import {Guard} from "../../core/logic/Guard";
import {AppError} from "./Exceptions/AppError";


interface CodeProps {
    value: string;
}

export class Code extends ValueObject<CodeProps> {

    get value(): string {
        return this.props.value;
    }

    private constructor(props: CodeProps) {
        super(props);
    }

    public static create(code: string): Result<Code> {

        const guardResult = Guard.againstNullOrUndefined(code, "code");
        if (!guardResult.succeeded) {
            throw new AppError("CODE_NULL_UNDEFINED");
        }

        const trimmedCode = code.trim();
        
        this.validateICD11Code(trimmedCode);
        
        return Result.ok<Code>(new Code({value: trimmedCode}))

    }

    private static validateICD11Code(code: string): void {
        
        
        
        console.log("Code:" + code);
        
        const icd11Regex = /^[A-Z]{1,2}[0-9]{2,3}(\.[0-9]{1,2})?$/;

        if (!code || /\s/.test(code)) {
            throw new AppError("CODE_INVALID_WHITESPACE");
        }
        

        // Testa o código com a expressão regular
        if (!icd11Regex.test(code)) {
            throw new AppError("INVALID_ICD11_CODE");
        }
    }


}