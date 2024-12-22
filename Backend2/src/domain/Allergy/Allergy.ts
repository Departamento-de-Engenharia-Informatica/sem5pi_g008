import {AggregateRoot} from "../../core/domain/AggregateRoot";
import {UniqueEntityID} from "../../core/domain/UniqueEntityID";
import {AllergyId} from "./AllergyId";
import {Result} from "../../core/logic/Result";
import {ObjectId} from "mongodb";

export interface AllergyProps {
    _id?: string;
    allergy: string;
    effect?: string;
    isDeleted?: boolean;
}

export class Allergy extends AggregateRoot<AllergyProps> {

    get id(): UniqueEntityID {
        return this._id;
    }

    get domainId(): AllergyId {
        return new AllergyId(this.id);
    }

    get allergy(): string {
        return this.props.allergy;
    }

    set allergy(value: string) {
        this.props.allergy = value;
    }

    get effect(): string {
        return this.props.effect;
    }

    set effect(value: string) {
        this.props.effect = value;
    }

    get isDeleted(): boolean {
        return this.props.isDeleted;
    }

    public deleteAllergy(): void {
        this.props.isDeleted = true;
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
}

