import {Repo} from "../../core/infra/Repo";
import {MedicalRecordFreeText} from "../../domain/MedicalRecordFreeText/MedicalRecordFreeText";
import {MedicalRecordAllergy} from "../../domain/MedicalRecordAllergy/MedicalRecordAllergy";

export default interface IMedicalRecordFreeTextRepo extends Repo<MedicalRecordFreeText> {
  save(medicalRecordFreeText: MedicalRecordFreeText): Promise<MedicalRecordFreeText>;
  getByMedicalId(medicalRecordId: string): Promise<MedicalRecordFreeText[]>;

}
