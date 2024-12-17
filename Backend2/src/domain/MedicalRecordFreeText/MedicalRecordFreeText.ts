import {AggregateRoot} from "../../core/domain/AggregateRoot";
import {UniqueEntityID} from "../../core/domain/UniqueEntityID";
import {Result} from "../../core/logic/Result";

interface MedicalRecordFreeTextProps {
  medicalRecord: string;
  doctorId: string;
  comment: string;
}

export class MedicalRecordFreeText extends AggregateRoot<MedicalRecordFreeTextProps>{

  get id(): UniqueEntityID {
    return this._id;
  }

  get domainId(): number {
    return <number>this.id.toValue();
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

  private constructor(props: MedicalRecordFreeTextProps, id?: UniqueEntityID) {
    super(props, id);
  }

  public static create(props: MedicalRecordFreeTextProps, id?: UniqueEntityID): Result<MedicalRecordFreeText> {

    try {
      const medicalRecordFreeText = new MedicalRecordFreeText(props, id);
      return Result.ok<MedicalRecordFreeText>(medicalRecordFreeText);

    } catch (e) {
      return Result.fail<MedicalRecordFreeText>(e.message);
    }
  }
}
