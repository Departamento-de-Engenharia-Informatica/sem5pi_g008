import {ValueObject} from "../../core/domain/ValueObject";
import {Result} from "../../core/logic/Result";
import {Guard} from "../../core/logic/Guard";
import {AppError} from "./Exceptions/AppError";


interface DescriptionProps {
    value: string;
}

export class Description extends ValueObject<DescriptionProps> {

    get value(): string {
        return this.props.value;
    }

    private constructor(props: DescriptionProps) {
        super(props);
    }

    public static create(description: string): Result<Description> {

        const guardResult = Guard.againstNullOrUndefined(description, 'description');
        
        if (!guardResult.succeeded) {
            throw new AppError("DESCRIPTION_NULL_UNDEFINED"); 
        }
        
        if(description.trim().length > 2048) {
            throw new AppError("DESCRIPTION_INVALID_LENGTH");
        }

        return Result.ok<Description>(new Description({value: description}));
    }
}