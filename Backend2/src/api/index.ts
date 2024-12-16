import { Router } from 'express';
import allergy from './routes/allergyRoute';

export default () => {
	const app = Router();

  allergy(app);

	return app
}
