import {AggregateRoot} from "../../core/domain/AggregateRoot";
import {UniqueEntityID} from "../../core/domain/UniqueEntityID";
import {MedicalRecordId} from "./MedicalRecordId";
import {Result} from "../../core/logic/Result";
import {Allergy} from "../Allergy/Allergy";
import {MedicalCondition} from "../MedicalCondition/MedicalCondition";


interface MedicalRecordProps{
    allergies: Allergy[];
    medicalConditions: MedicalCondition[];
    freeText: string;
}

export class MedicalRecord extends AggregateRoot<MedicalRecordProps>{
    
    
    get id(): UniqueEntityID{
        return this._id;
    }
    
    get domainId(): MedicalRecordId {
        return MedicalRecordId.caller(this.id);
    }
    
    private constructor(props: MedicalRecordProps, id?:UniqueEntityID) {
        super(props, id);
    }
    
    public static create(props:MedicalRecordProps, id?: UniqueEntityID): Result<MedicalRecord> {
        const medRecord= new MedicalRecord(props, id);
        
        return Result.ok<MedicalRecord>(medRecord);
    }
    
  
}