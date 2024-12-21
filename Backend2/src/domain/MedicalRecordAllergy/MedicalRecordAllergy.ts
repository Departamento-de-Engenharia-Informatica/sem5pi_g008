import {Allergy} from "../Allergy/Allergy";
import {AggregateRoot} from "../../core/domain/AggregateRoot";
import {UniqueEntityID} from "../../core/domain/UniqueEntityID";
import {Result} from "../../core/logic/Result";

interface MedicalRecordAllergyProps {
  allergy: string;
  medicalRecord: string;
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
    return this.props.allergy;
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
