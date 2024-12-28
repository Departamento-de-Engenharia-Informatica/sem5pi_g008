import {AggregateRoot} from "../../core/domain/AggregateRoot";
import {UniqueEntityID} from "../../core/domain/UniqueEntityID";
import {AllergyId} from "./AllergyId";
import {Result} from "../../core/logic/Result";
import {ObjectId} from "mongodb";
import {Code} from "../MedicalCondition/code";
import {Designation} from "../MedicalCondition/designation";
import {Description} from "../MedicalCondition/description";

export interface AllergyProps {
  _id?: string;
  code: Code;
  designation: Designation;
  description: Description;
  effects: string[];
  isDeleted?: boolean;
}

export class Allergy extends AggregateRoot<AllergyProps> {

    get id(): UniqueEntityID {
        return this._id;
    }

    get domainId(): AllergyId {
        return new AllergyId(this.id);
    }

    get code(): string {
        return this.props.code.value;
    }

    get designation(): string {
        return this.props.designation.value;
    }

    get description(): string {
        return this.props.description.value;
    }

    get effects(): string[] {
        return this.props.effects;
    }

    get isDeleted(): boolean {
        return this.props.isDeleted;
    }

    set designation(newDesignation: Designation) {
        this.props.designation = newDesignation;
    }
    
    set description(newDescription: Description) {
        this.props.description = newDescription;
    }
    
    public updateEffects(newEffects: string[]): void {
        this.setEffectsList(newEffects);
    }
    
    private constructor(props: AllergyProps, id?: UniqueEntityID) {
        if (id) {
            new AllergyId(id);
        }

        if(props.isDeleted === undefined){
            props.isDeleted = false;
        }

        super(props, id);
    }

    public static create(props: AllergyProps, id?: UniqueEntityID): Result<Allergy> {
        try {
            const allergy = new Allergy(props, id);
            return Result.ok<Allergy>(allergy);

        } catch (e) {
            return Result.fail<Allergy>(e.message);
        }
    }

    private setEffectsList(effects: string[]): void {
        this.props.effects = effects;
    }
}

