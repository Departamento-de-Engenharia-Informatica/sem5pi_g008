import {ValueObject} from "../../core/domain/ValueObject";
import {Result} from "../../core/logic/Result";
import {Guard} from "../../core/logic/Guard";
import {AppError} from "./Exceptions/AppError";


interface DesignationProps {
    value: string;
}

export class Designation extends ValueObject<DesignationProps> {

    get value(): string {
        return this.props.value;
    }

    private constructor(props: DesignationProps) {
        super(props);
    }

    public static create(designation: string): Result<Designation> {

        const guardResult = Guard.againstNullOrUndefined(designation, 'designation');

        if (!guardResult.succeeded) {
            throw new AppError("DESIGNATION_NULL_UNDEFINED");
        }

        const trimmedDesignation = designation.trim();
        
        if(trimmedDesignation.length === 0) {
            throw new AppError("DESIGNATION_INVALID_WHITESPACE");
        }
        
        if (trimmedDesignation.length > 100) {
            throw new AppError("DESIGNATION_INVALID_LENGTH");
        }
        
        return Result.ok<Designation>(new Designation({value: trimmedDesignation}))
    }

}