import {Allergy} from "../Allergy/Allergy";
import {AggregateRoot} from "../../core/domain/AggregateRoot";
import {UniqueEntityID} from "../../core/domain/UniqueEntityID";

interface MedicalRecordFreeTextProps {
  doctorId: string;
  comment: string;
}

export class MedicalRecordFreeText extends AggregateRoot<MedicalRecordFreeTextProps>{

  get id(): UniqueEntityID {
    return this._id;
  }

  get domainId(): string {
    return this.id.toString();
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

  public static create(props: MedicalRecordFreeTextProps, id?: UniqueEntityID): MedicalRecordFreeText {
    const medRecordCondition = new MedicalRecordFreeText(props, id);
    return medRecordCondition;
  }
}
