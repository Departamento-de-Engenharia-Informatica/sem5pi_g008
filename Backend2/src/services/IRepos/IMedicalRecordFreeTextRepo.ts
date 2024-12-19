import {Repo} from "../../core/infra/Repo";
import {MedicalRecordFreeText} from "../../domain/MedicalRecordFreeText/MedicalRecordFreeText";

export default interface IMedicalRecordFreeTextRepo extends Repo<MedicalRecordFreeText> {
  save(medicalCondition: MedicalRecordFreeText): Promise<MedicalRecordFreeText>;
}
