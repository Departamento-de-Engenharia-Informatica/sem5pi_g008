import {AggregateRoot} from "../../core/domain/AggregateRoot";
import {UniqueEntityID} from "../../core/domain/UniqueEntityID";
import {Result} from "../../core/logic/Result";
import {MedicalConditionId} from "./MedicalConditionId";
import {Code} from "../Shared/code";
import {Designation} from "../Shared/designation";
import {Description} from "../Shared/description";

interface MedicalConditionProps {
    _id?: string;
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
      return new MedicalConditionId(this.id);
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

    set description(newDescription: Description) {
        this.props.description = newDescription;
    }

    get symptomsList(): string[] {
        return this.props.symptomsList;
    }

    public updateSymptomsList(symptomsList: string[]): void {
        this.setSymptomsList(symptomsList);
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

        if (!props.code || !props.designation || !props.description || !props.symptomsList) {
            return Result.fail<MedicalCondition>('Missing required properties');
        }        
        try {
            const medCond = new MedicalCondition(props, id);
            return Result.ok<MedicalCondition>(medCond);
        } catch (e) {
            return Result.fail<MedicalCondition>(e.message);
        }
    }

}
