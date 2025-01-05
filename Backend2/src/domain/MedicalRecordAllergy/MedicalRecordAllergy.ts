import {Allergy} from "../Allergy/Allergy";
import {AggregateRoot} from "../../core/domain/AggregateRoot";
import {UniqueEntityID} from "../../core/domain/UniqueEntityID";
import {Result} from "../../core/logic/Result";

interface MedicalRecordAllergyProps {
  allergyId: string;
  medicalRecordId: string;
  doctorId: string;
  comment: string;
}

export class MedicalRecordAllergy extends AggregateRoot<MedicalRecordAllergyProps>{

  get id(): UniqueEntityID {
    return this._id;
  }

  get domainId(): number {
    return <number>this.id.toValue();
  }

  get allergy(): string {
    return this.props.allergyId;
  }

  get medicalRecord(): string {
    return this.props.medicalRecordId;
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
  private constructor(props: MedicalRecordAllergyProps, id?: UniqueEntityID) {
    super(props, id);
  }

  public static create(props: MedicalRecordAllergyProps, id?: UniqueEntityID): Result<MedicalRecordAllergy> {

    try {
      const medicalRecordAllergy = new MedicalRecordAllergy(props, id);
      return Result.ok<MedicalRecordAllergy>(medicalRecordAllergy);

    } catch (e) {
      return Result.fail<MedicalRecordAllergy>(e.message);
    }
  }
}
