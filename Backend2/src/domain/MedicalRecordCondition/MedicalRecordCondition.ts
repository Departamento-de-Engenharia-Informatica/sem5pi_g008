import {AggregateRoot} from "../../core/domain/AggregateRoot";
import {UniqueEntityID} from "../../core/domain/UniqueEntityID";
import {Result} from "../../core/logic/Result";

interface MedicalRecordConditionProps {
  condition: string;
  medicalRecord: string;
  doctorId: string;
  comment: string;
}

export class MedicalRecordCondition extends AggregateRoot<MedicalRecordConditionProps> {

  get id(): UniqueEntityID {
    return this._id;
  }

  get domainId(): number {
    return <number>this.id.toValue();
  }

  get condition(): string {
    return this.props.condition;
  }

  get medicalRecord(): string {
    return this.props.medicalRecord;
  }

  get doctorId(): string {
    return this.props.doctorId;
  }

  get comment(): string {
    return this.props.comment;
  }
  set comment(newComment: string) {
    this.props.comment = newComment
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
