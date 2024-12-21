import {AggregateRoot} from "../../core/domain/AggregateRoot";
import {UniqueEntityID} from "../../core/domain/UniqueEntityID";
import {Result} from "../../core/logic/Result";
import {MedicalConditionId} from "./MedicalConditionId";
import {Code} from "./code";
import {Designation} from "./designation";
import {Description} from "./description";

interface MedicalConditionProps {

    code: Code;
    designation: Designation;
    description: Description;
    symptomsList: string[];
}

export class MedicalCondition extends AggregateRoot<MedicalConditionProps> {

    get id(): UniqueEntityID {
        return this._id;
    }

    get domainId(): MedicalConditionId {
        return MedicalConditionId.caller(this.id);
    }

    get code(): Code {
        return this.props.code;
    }
    
    get designation(): Designation {
        return this.props.designation;
    }
    
    get description(): Description {
        return this.props.description;
    }
    
    get symptomsList(): string[] {
        return this.props.symptomsList;
    }
    
    private setCode(code: Code): void {
        this.props.code = code;
    }
    
    private setDesignation(designation: Designation): void {
        this.props.designation = designation;
    }
    
    private setDescription(description: Description): void {
        this.props.description = description;
    }
    
    private setSymptomsList(symptomsList: string[]): void {
        this.props.symptomsList = symptomsList;
    }
    

    private constructor(props: MedicalConditionProps, id?: UniqueEntityID) {
        if (id) {
            new MedicalConditionId(id);
        }
        super(props, id);
    }

    public static create(props: MedicalConditionProps, id?: UniqueEntityID): Result<MedicalCondition> {

        try {
            const medCond = new MedicalCondition(props, id);
            return Result.ok<MedicalCondition>(medCond);
        } catch (e) {
            return Result.fail<MedicalCondition>(e.message);
        }
    }

}
