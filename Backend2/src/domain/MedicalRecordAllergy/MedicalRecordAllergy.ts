import {Allergy} from "../Allergy/Allergy";
import {AggregateRoot} from "../../core/domain/AggregateRoot";
import {UniqueEntityID} from "../../core/domain/UniqueEntityID";

interface MedicalRecordAllergyProps {
  allergy: Allergy;
  doctorId: string;
  comment: string;
}

export class MedicalRecordAllergy extends AggregateRoot<MedicalRecordAllergyProps>{

  get id(): UniqueEntityID {
    return this._id;
  }

  get domainId(): string {
    return this.id.toString();
  }

  get allergy(): Allergy {
    return this.props.allergy;
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

  public static create(props: MedicalRecordAllergyProps, id?: UniqueEntityID): MedicalRecordAllergy {
    const medRecordAllergy = new MedicalRecordAllergy(props, id);
    return medRecordAllergy;
  }
}
