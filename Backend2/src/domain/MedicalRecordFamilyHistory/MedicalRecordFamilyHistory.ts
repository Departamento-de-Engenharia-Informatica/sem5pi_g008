import { AggregateRoot } from "../../core/domain/AggregateRoot";
import { UniqueEntityID } from "../../core/domain/UniqueEntityID";
import { Result } from "../../core/logic/Result";
import {MedicalRecord} from "../MedicalRecord/MedicalRecord";

interface MedicalRecordFamilyHistoryProps {
    medicalRecord: string;
    familyMember: string; // e.g., "Father", "Mother", "Sibling"
    condition: string; // e.g., "Diabetes", "Hypertension"LISTA
}

export class MedicalRecordFamilyHistory extends AggregateRoot<MedicalRecordFamilyHistoryProps> {
    get id(): UniqueEntityID {
        return this._id;
    }

    get domainId(): number {
        return <number>this.id.toValue();
    }

    get medicalRecord(): string { 
        return this.props.medicalRecord;
    }

    get familyMember(): string {
        return this.props.familyMember;
    }

    get condition(): string {
        return this.props.condition;
    }

    private constructor(props: MedicalRecordFamilyHistoryProps, id?: UniqueEntityID) {
        super(props, id);
    }

    public static create(props: MedicalRecordFamilyHistoryProps, id?: UniqueEntityID): Result<MedicalRecordFamilyHistory> {
        try {
            // You can add validation logic here if required
            const familyHistory = new MedicalRecordFamilyHistory(props, id);
            return Result.ok<MedicalRecordFamilyHistory>(familyHistory);
        } catch (e) {
            return Result.fail<MedicalRecordFamilyHistory>(e.message);
        }
    }
    
}
