import {Allergy} from "../Allergy/Allergy";
import {AggregateRoot} from "../../core/domain/AggregateRoot";
import {UniqueEntityID} from "../../core/domain/UniqueEntityID";
import {MedicalCondition} from "../MedicalCondition/MedicalCondition";
import {Result} from "../../core/logic/Result";
import {MedicalRecord} from "../MedicalRecord/MedicalRecord";

interface MedicalRecordConditionProps {
  condition: MedicalCondition;
  medicalRecord: MedicalRecord;
  doctorId: string;
  comment: string;
}

export class MedicalRecordCondition extends AggregateRoot<MedicalRecordConditionProps> {

  get id(): UniqueEntityID {
    return this._id;
  }

  get domainId(): string {
    return this.id.toString();
  }

  get condition(): MedicalCondition {
    return this.props.condition;
  }

  get doctorId(): string {
    return this.props.doctorId;
  }

  get comment(): string {
    return this.props.comment;
  }

  private constructor(props: MedicalRecordConditionProps, id?: UniqueEntityID) {
    super(props, id);
  }

  public static create(props: MedicalRecordConditionProps, id?: UniqueEntityID): Result<MedicalRecordCondition> {
    try {
      const medicalRecordAllergy = new MedicalRecordCondition(props, id);
      return Result.ok<MedicalRecordCondition>(medicalRecordAllergy);

    } catch (e) {
      return Result.fail<MedicalRecordCondition>(e.message);
    }
  }
}
