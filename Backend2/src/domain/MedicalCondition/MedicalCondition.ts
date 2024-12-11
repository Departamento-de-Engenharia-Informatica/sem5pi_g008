import {AggregateRoot} from "../../core/domain/AggregateRoot";
import {UniqueEntityID} from "../../core/domain/UniqueEntityID";
import {Result} from "../../core/logic/Result";
import {MedicalConditionId} from "./MedicalConditionId";

interface MedicalConditionProps {
  condition: string;
}

export class MedicalCondition extends AggregateRoot<MedicalConditionProps> {
    
    get id(): UniqueEntityID {
        return this._id;
    }
    
    get domainId(): MedicalConditionId {
        return MedicalConditionId.caller(this.id);
    }
    
    get condition(): string {
        return this.props.condition;
    }
    
    set condition(value: string) {
        this.props.condition = value;
    }

    private constructor (props: MedicalConditionProps, id?: UniqueEntityID) {
        super(props, id);
    }
    
    public static create (props: MedicalConditionProps, id?: UniqueEntityID): Result<MedicalCondition> {
        const medCond = new MedicalCondition(props, id);
        
        return Result.ok<MedicalCondition>(medCond);
    }
    
}