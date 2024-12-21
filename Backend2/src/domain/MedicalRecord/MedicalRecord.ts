import {AggregateRoot} from "../../core/domain/AggregateRoot";
import {UniqueEntityID} from "../../core/domain/UniqueEntityID";
import {MedicalRecordId} from "./MedicalRecordId";
import {Result} from "../../core/logic/Result";
import {MedicalRecordAllergy} from "../MedicalRecordAllergy/MedicalRecordAllergy";
import {MedicalRecordCondition} from "../MedicalRecordCondition/MedicalRecordCondition";
import {MedicalRecordFreeText} from "../MedicalRecordFreeText/MedicalRecordFreeText";
import {ObjectId} from "mongodb";


interface MedicalRecordProps {
  _id?: string;
  medicalRecordConditions?: MedicalRecordCondition[];
  medicalRecordAllergies?: MedicalRecordAllergy[];
  freeText?: MedicalRecordFreeText[];
}

export class MedicalRecord extends AggregateRoot<MedicalRecordProps> {

  get id(): UniqueEntityID {
    return this._id;
  }

  get domainId(): MedicalRecordId {
    return MedicalRecordId.caller(this.id);
  }

  getPrivateId(): string {
    return this.props._id;
  }

  get medicalRecordConditions(): MedicalRecordCondition[] {
    return this.props.medicalRecordConditions;
  }

  get medicalRecordAllergies(): MedicalRecordAllergy[] {
    return this.props.medicalRecordAllergies;
  }

  get freeText(): MedicalRecordFreeText[] {
    return this.props.freeText;
  }

  set medicalRecordConditions(value: MedicalRecordCondition[]) {
    this.props.medicalRecordConditions = value;
  }

  set medicalRecordAllergies(value: MedicalRecordAllergy[]) {
    this.props.medicalRecordAllergies = value;
  }

  set freeText(value: MedicalRecordFreeText[]) {
    this.props.freeText = value;
  }

  private constructor(props: MedicalRecordProps, id?: UniqueEntityID) {
    super(props, id);
  }

  public static create(props: MedicalRecordProps, id?: UniqueEntityID): Result<MedicalRecord> {
    const medRecord = new MedicalRecord(props, id);

    return Result.ok<MedicalRecord>(medRecord);
  }


}
